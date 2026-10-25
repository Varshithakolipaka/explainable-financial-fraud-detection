# app.R
# Explainable Financial Fraud Detection Using Transaction Behaviour Analytics
# Basic-package version: dplyr, tidyverse, ranger, scales + Shiny UI package.
# Put the PaySim CSV beside app.R to avoid browser upload limits.

required_packages <- c("shiny", "tidyverse", "ranger", "scales", "lime")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}

library(shiny)
library(tidyverse)
library(ranger)
library(scales)
library(lime)

# Increase browser upload limit (local R session memory still matters).
options(shiny.maxRequestSize = 2 * 1024^3) # 2 GB

DATA_FILE <- "PS_20174392719_1491204439457_log.csv"
MAX_SAMPLE <- 250000

find_local_csv <- function() {
  if (file.exists(DATA_FILE)) return(DATA_FILE)
  csvs <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
  if (length(csvs) == 1) return(csvs[[1]])
  NA_character_
}

find_local_csv <- function() {
  if (file.exists(DATA_FILE)) return(DATA_FILE)
  csvs <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
  csvs <- csvs[basename(csvs) != ""]
  if (length(csvs) == 1) return(csvs[[1]])
  NA_character_
}
set.seed(42)

prepare_data <- function(raw) {
  needed <- c("step", "type", "amount", "oldbalanceOrg",
              "newbalanceOrig", "oldbalanceDest", "newbalanceDest", "isFraud")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) stop("Dataset missing columns: ", paste(missing, collapse = ", "))

  raw %>%
    transmute(
      step = as.numeric(step),
      type = as.character(type),
      amount = as.numeric(amount),
      oldbalanceOrg = as.numeric(oldbalanceOrg),
      newbalanceOrig = as.numeric(newbalanceOrig),
      oldbalanceDest = as.numeric(oldbalanceDest),
      newbalanceDest = as.numeric(newbalanceDest),
      isFraud = as.integer(isFraud)
    ) %>%
    filter(
      if_all(everything(), ~ !is.na(.x)),
      step >= 0, amount >= 0, isFraud %in% c(0L, 1L)
    ) %>%
    distinct() %>%
    mutate(
      type = factor(type),
      hour = step %% 24,
      log_amount = log1p(amount),
      errorBalanceOrig = newbalanceOrig + amount - oldbalanceOrg,
      errorBalanceDest = oldbalanceDest + amount - newbalanceDest,
      amount_ratio = amount / pmax(oldbalanceOrg, 1),
      emptied = as.integer(newbalanceOrig == 0),
      Class = factor(if_else(isFraud == 1, "Fraud", "Legitimate"),
                     levels = c("Legitimate", "Fraud"))
    )
}


explain_fraud_cases <- function(d) {
  # These are observable behavioural signals, not proof of why a transaction was fraudulent.
  d %>%
    filter(isFraud == 1) %>%
    mutate(
      reason_balance_mismatch = abs(errorBalanceOrig) > 1 |
        abs(errorBalanceDest) > 1,
      reason_sender_emptied = emptied == 1,
      reason_large_relative_to_balance = amount_ratio >= 1,
      reason_transfer_or_cashout = as.character(type) %in% c("TRANSFER", "CASH_OUT"),
      reason_high_amount = amount >= quantile(d$amount, 0.95, na.rm = TRUE),
      explanation = pmap_chr(
        list(reason_balance_mismatch, reason_sender_emptied,
             reason_large_relative_to_balance, reason_transfer_or_cashout,
             reason_high_amount),
        function(balance_mismatch, sender_emptied, large_relative, transfer_cashout, high_amount) {
          flags <- c(
            if (balance_mismatch) "Balance mismatch",
            if (sender_emptied) "Sender balance became zero",
            if (large_relative) "Amount >= sender's previous balance",
            if (transfer_cashout) "Transfer/cash-out transaction",
            if (high_amount) "High amount (top 5% of sample)"
          )
          if (!length(flags)) "No listed rule triggered; label comes from dataset" else
            paste(flags, collapse = "; ")
        }
      )
    )
}

split_data <- function(d) {
  train_idx <- unlist(lapply(split(seq_len(nrow(d)), d$Class), function(idx) {
    sample(idx, max(1, floor(0.8 * length(idx))))
  }))
  list(train = d[train_idx, , drop = FALSE],
       test = d[-train_idx, , drop = FALSE])
}

train_model <- function(raw) {
  d <- prepare_data(raw)
  if (nrow(d) < 100 || n_distinct(d$Class) < 2) {
    stop("Need at least 100 valid rows and both target classes.")
  }
  # Keep every fraud-labelled record for the case-analysis tab; sample only
  # legitimate records when building the model-analysis sample.
  fraud_all <- filter(d, isFraud == 1)
  legit_all <- filter(d, isFraud == 0)
  if (nrow(d) > MAX_SAMPLE) {
    fraud_keep <- fraud_all
    legit_n <- max(1, MAX_SAMPLE - nrow(fraud_keep))
    if (nrow(legit_all) > legit_n) legit_all <- slice_sample(legit_all, n = legit_n)
    d <- bind_rows(fraud_keep, legit_all) %>% slice_sample(prop = 1)
  }

  sp <- split_data(d)
  train <- sp$train
  test <- sp$test

  # Downsample majority class in training only; test split stays at natural sampled prevalence.
  fraud_rows <- filter(train, Class == "Fraud")
  legit_rows <- filter(train, Class == "Legitimate")
  if (nrow(fraud_rows) < 5) stop("Too few fraud rows. Increase MAX_SAMPLE.")
  legit_n <- min(nrow(legit_rows), max(500, 10 * nrow(fraud_rows)))
  model_data <- bind_rows(fraud_rows, slice_sample(legit_rows, n = legit_n)) %>%
    slice_sample(prop = 1)

  predictors <- c("step", "type", "amount", "oldbalanceOrg", "newbalanceOrig",
                  "oldbalanceDest", "newbalanceDest", "hour", "log_amount",
                  "errorBalanceOrig", "errorBalanceDest", "amount_ratio", "emptied")
  model <- ranger(
    formula = as.formula(paste("Class ~", paste(predictors, collapse = " + "))),
    data = select(model_data, all_of(c("Class", predictors))),
    probability = TRUE, num.trees = 200,
    importance = "permutation", seed = 42,
    respect.unordered.factors = "order"
  )
  test_x <- select(test, all_of(predictors))
  prob <- predict(model, data = test_x)$predictions[, "Fraud"]
  threshold <- 0.30
  pred <- factor(if_else(prob >= threshold, "Fraud", "Legitimate"),
                 levels = c("Legitimate", "Fraud"))
  results <- mutate(test, fraud_probability = prob, predicted = pred)
  cm <- table(
    Actual = factor(results$Class, levels = c("Legitimate", "Fraud")),
    Predicted = factor(results$predicted, levels = c("Legitimate", "Fraud"))
  )
  tp <- unname(cm["Fraud", "Fraud"]); fp <- unname(cm["Legitimate", "Fraud"])
  fn <- unname(cm["Fraud", "Legitimate"]); tn <- unname(cm["Legitimate", "Legitimate"])
  precision <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
  f1 <- ifelse(is.na(precision) || is.na(recall) || precision + recall == 0,
               NA_real_, 2 * precision * recall / (precision + recall))
  metrics <- tibble(
    Metric = c("Accuracy", "Precision", "Recall", "F1-score"),
    Value = c((tp + tn) / sum(cm), precision, recall, f1)
  )
  list(data = d, model = model, predictors = predictors, results = results,
       cm = cm, metrics = metrics, threshold = threshold)
}

# SHAP-like local explanation using only base R + ranger:
# This is NOT exact SHAP. It reports permutation importance globally and a
# transparent one-feature-at-a-time probability change locally, with other
# features held at their selected values. The UI labels this limitation clearly.
# LIME explanations are generated below using lime::lime() and lime::explain().
# The ranger adapter returns class probabilities for the LIME-generated samples.
model_type.ranger <- function(x, ...) "classification"
predict_model.ranger <- function(x, newdata, type, ...) {
  predict(x, data = newdata)$predictions
}


ui <- fluidPage(
  titlePanel("Explainable Financial Fraud Detection"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Load PaySim dataset"),
      p("Recommended: place the CSV beside app.R and load directly. Upload is optional."),
      fileInput("file", "Upload CSV (if small enough)", accept = ".csv"),
      actionButton("load", "Load data and train model"),
      hr(),
      h4("2. Test a transaction"),
      numericInput("step", "Step (hour index)", 100, min = 0),
      selectInput("type", "Transaction type",
                  choices = c("TRANSFER", "CASH_OUT", "PAYMENT", "CASH_IN", "DEBIT")),
      numericInput("amount", "Amount", 2500, min = 0),
      numericInput("oldOrg", "Sender balance before", 2500, min = 0),
      numericInput("newOrg", "Sender balance after", 0, min = 0),
      numericInput("oldDest", "Receiver balance before", 0, min = 0),
      numericInput("newDest", "Receiver balance after", 2500, min = 0),
      numericInput("threshold", "Fraud threshold", 0.30, min = 0.01, max = 0.99, step = 0.05),
      actionButton("predict", "Predict and explain")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
          uiOutput("status"),
          fluidRow(
            column(3, wellPanel(h4("Transactions"), textOutput("n"))),
            column(3, wellPanel(h4("Fraud cases"), textOutput("frauds"))),
            column(3, wellPanel(h4("Fraud rate"), textOutput("rate"))),
            column(3, wellPanel(h4("Fraud amount"), textOutput("fraud_amount")))
          ),
          h4("Fraud vs legitimate"), plotOutput("class_plot"),
          h4("Fraud count by transaction type"), plotOutput("type_plot"),
          h4("Transaction amount by class"), plotOutput("amount_plot")
        ),
        tabPanel("Fraud Case Analysis",
          h4("Fraud-labeled transactions in the analyzed dataset"),
          p("The table lists records whose original dataset label is isFraud = 1. The explanation column identifies observable behaviour signals; it does not prove the real-world cause of fraud."),
          uiOutput("fraud_case_summary"),
          h4("Count of fraud-labelled records by behaviour signal"),
          plotOutput("fraud_reason_plot"),
          h4("Fraud cases and observable signals"),
          tableOutput("fraud_cases")
        ),
        tabPanel("Model Evaluation",
          h4("Held-out test metrics"), tableOutput("metrics"),
          h4("Confusion matrix"), tableOutput("confusion"),
          h4("Global permutation feature importance"), plotOutput("importance"),
          h4("Sample predictions"), tableOutput("predictions")
        ),
        tabPanel("Transaction Explanation",
          uiOutput("prediction_result"),
          h4("Why the model flagged this transaction"),
          p("LIME fits a simple local surrogate model around this transaction. The bars show which feature conditions support or oppose the Random Forest prediction near this specific case."),
          plotOutput("local_plot"),
          h4("LIME explanation details"),
          tableOutput("lime_table"),
          p("Positive weights support the selected class (Fraud); negative weights oppose it. Feature conditions are local approximations, not proof of criminal intent or causation. Explanations depend on the model, data sample, and LIME settings.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  fitted <- reactiveVal(NULL)
  status_msg <- reactiveVal("Place the PaySim CSV beside app.R or upload it, then click Load data and train model.")
  prediction <- reactiveVal(NULL)
  lime_explainer <- reactiveVal(NULL)

  observeEvent(input$load, {
    path <- if (!is.null(input$file)) input$file$datapath else find_local_csv()
    if (is.na(path) || !file.exists(path)) {
      status_msg(paste0(
        "CSV not found or more than one CSV is present. Put the PaySim CSV beside app.R ",
        "and keep the expected filename ", DATA_FILE,
        ", or leave only the PaySim CSV in this folder, then click Load data and train model."
      ))
      return()
    }
    tryCatch({
      withProgress(message = "Preparing data and training model", value = 0, {
        raw <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
        incProgress(0.35, detail = "Cleaning and engineering features")
        obj <- train_model(raw)
        incProgress(0.65, detail = "Preparing LIME explainer")
        # Use analysed data as the neighbourhood reference for local explanations.
        explainer <- lime::lime(
          x = obj$data %>% select(all_of(obj$predictors)),
          model = obj$model,
          bin_continuous = TRUE
        )
        fitted(obj)
        lime_explainer(explainer)
      })
      status_msg("Model trained successfully.")
    }, error = function(e) status_msg(paste("Error:", conditionMessage(e))))
  })

  output$status <- renderUI(div(class = "well", status_msg()))
  output$n <- renderText({ req(fitted()); comma(nrow(fitted()$data)) })
  output$frauds <- renderText({ req(fitted()); comma(sum(fitted()$data$isFraud == 1)) })
  output$rate <- renderText({ req(fitted()); percent(mean(fitted()$data$isFraud == 1), accuracy = 0.01) })
  output$fraud_amount <- renderText({
    req(fitted()); dollar(sum(fitted()$data$amount[fitted()$data$isFraud == 1], na.rm = TRUE))
  })

  output$class_plot <- renderPlot({
    req(fitted())
    fitted()$data %>% count(Class) %>%
      ggplot(aes(Class, n, fill = Class)) + geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = comma) +
      labs(title = "Transaction class distribution", x = "Class", y = "Transactions")
  })
  output$type_plot <- renderPlot({
    req(fitted())
    fitted()$data %>% filter(isFraud == 1) %>% count(type, sort = TRUE) %>%
      ggplot(aes(reorder(type, n), n)) + geom_col() + coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(title = "Fraud counts by transaction type", x = "Type", y = "Fraud count")
  })
  output$amount_plot <- renderPlot({
    req(fitted())
    cap <- quantile(fitted()$data$amount, 0.99, na.rm = TRUE)
    fitted()$data %>% mutate(amount_cap = pmin(amount, cap)) %>%
      ggplot(aes(Class, amount_cap, fill = Class)) + geom_boxplot(outlier.alpha = 0.05) +
      labs(title = "Transaction amount by class (capped at 99th percentile)",
           x = "Class", y = "Amount")
  })
  fraud_cases_data <- reactive({
    req(fitted())
    explain_fraud_cases(fitted()$data)
  })
  output$fraud_case_summary <- renderUI({
    req(fitted())
    fc <- fraud_cases_data()
    div(class = "well",
        h4(paste0(format(nrow(fc), big.mark = ","), " fraud-labelled cases found in the analyzed sample")),
        p("If your full CSV has 333 fraud rows, the count will show 333 only when all 333 are included in the analyzed sample. The app samples up to 250,000 rows for speed, so a sample can contain fewer cases.")
    )
  })
  output$fraud_reason_plot <- renderPlot({
    fc <- fraud_cases_data()
    req(nrow(fc) > 0)
    reason_counts <- tibble(
      Reason = c("Balance mismatch", "Sender balance became zero",
                 "Amount >= sender previous balance", "Transfer/cash-out",
                 "High amount (top 5%)"),
      Count = c(sum(fc$reason_balance_mismatch, na.rm = TRUE),
                sum(fc$reason_sender_emptied, na.rm = TRUE),
                sum(fc$reason_large_relative_to_balance, na.rm = TRUE),
                sum(fc$reason_transfer_or_cashout, na.rm = TRUE),
                sum(fc$reason_high_amount, na.rm = TRUE))
    )
    ggplot(reason_counts, aes(Count, reorder(Reason, Count))) +
      geom_col() + scale_x_continuous(labels = comma) +
      labs(title = "Observable signals among fraud-labelled transactions",
           x = "Number of records with signal", y = "Behaviour signal")
  })
  output$fraud_cases <- renderTable({
    fc <- fraud_cases_data()
    fc %>%
      select(step, type, amount, oldbalanceOrg, newbalanceOrig,
             oldbalanceDest, newbalanceDest, reason_balance_mismatch,
             reason_sender_emptied, reason_large_relative_to_balance,
             reason_transfer_or_cashout, reason_high_amount, explanation) %>%
      slice_head(n = 333) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  output$metrics <- renderTable({
    req(fitted())
    fitted()$metrics %>% mutate(Value = if_else(is.na(Value), "NA", percent(Value, accuracy = 0.01)))
  })
  output$confusion <- renderTable({
    req(fitted())
    as.data.frame.matrix(fitted()$cm) %>% rownames_to_column("Actual")
  })
  output$importance <- renderPlot({
    req(fitted())
    enframe(fitted()$model$variable.importance, name = "Feature", value = "Importance") %>%
      ggplot(aes(Importance, reorder(Feature, Importance))) +
      geom_col() + labs(x = "Permutation importance", y = "Feature",
                        title = "Global model feature importance")
  })
  output$predictions <- renderTable({
    req(fitted())
    fitted()$results %>%
      select(step, type, amount, Class, predicted, fraud_probability) %>%
      slice_head(n = 15) %>%
      mutate(fraud_probability = round(fraud_probability, 4))
  })

  observeEvent(input$predict, {
    req(fitted())
    type_levels <- levels(fitted()$data$type)
    typ <- if (input$type %in% type_levels) input$type else type_levels[1]
    x <- tibble(
      step = input$step, type = factor(typ, levels = type_levels),
      amount = input$amount, oldbalanceOrg = input$oldOrg,
      newbalanceOrig = input$newOrg, oldbalanceDest = input$oldDest,
      newbalanceDest = input$newDest, hour = input$step %% 24,
      log_amount = log1p(input$amount),
      errorBalanceOrig = input$newOrg + input$amount - input$oldOrg,
      errorBalanceDest = input$oldDest + input$amount - input$newDest,
      amount_ratio = input$amount / max(input$oldOrg, 1),
      emptied = as.integer(input$newOrg == 0)
    ) %>% select(all_of(fitted()$predictors))
    p <- predict(fitted()$model, data = x)$predictions[1, "Fraud"]
    # Generate a genuine LIME local explanation for the entered transaction.
    lime_result <- tryCatch(
      lime::explain(
        x = x,
        explainer = lime_explainer(),
        labels = "Fraud",
        n_features = 8,
        n_permutations = 1000
      ),
      error = function(e) {
        showNotification(paste("LIME explanation failed:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
    prediction(list(prob = p, threshold = input$threshold, x = x,
                    lime_result = lime_result))
  })
  output$prediction_result <- renderUI({
    p <- prediction()
    if (is.null(p)) return(div(class = "well", "Enter transaction values and click Predict and explain."))
    div(class = if (p$prob >= p$threshold) "alert alert-danger" else "alert alert-success",
        h4(if (p$prob >= p$threshold) "Flagged for fraud review" else "Likely legitimate"),
        p("Estimated fraud probability: ", percent(p$prob, accuracy = 0.01)),
        p("This is a model estimate, not proof of fraud."))
  })
  output$local_plot <- renderPlot({
    p <- prediction()
    req(p, p$lime_result)
    lime::plot_features(p$lime_result) +
      ggplot2::labs(
        title = "LIME: local explanation for this transaction",
        x = "Local feature weight",
        y = "Feature condition"
      )
  })
  output$lime_table <- renderTable({
    p <- prediction()
    req(p, p$lime_result)
    p$lime_result %>%
      select(feature, feature_value, feature_weight, label) %>%
      arrange(desc(abs(feature_weight))) %>%
      mutate(
        Direction = if_else(feature_weight >= 0, "Supports Fraud prediction",
                            "Opposes Fraud prediction"),
        feature_weight = round(feature_weight, 4)
      ) %>%
      select(Feature = feature, `Transaction value / condition` = feature_value,
             `LIME weight` = feature_weight, Direction)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

}

shinyApp(ui, server)
