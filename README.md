# Explainable Financial Fraud Detection Using Transaction Behaviour Analytics

An R Shiny dashboard that analyzes the PaySim synthetic mobile-money transaction dataset, trains a Random Forest classifier with `ranger`, evaluates its performance, and explains individual predictions using `lime`.

## Dashboard features

- Load the PaySim CSV from a local file or use the optional upload control when the file size permits.
- Summarize analyzed transactions, fraud-labelled cases, fraud rate, and total fraud-labelled amount.
- Explore fraud vs. legitimate transactions, transaction types, and amount distributions.
- Inspect fraud-labelled records and observable behaviour signals.
- Evaluate the classifier using accuracy, precision, recall, F1-score, a confusion matrix, and global permutation feature importance.
- Enter a transaction and view the estimated fraud probability and a local LIME explanation.

## Dataset

**PaySim — Synthetic Financial Datasets for Fraud Detection**

Kaggle: https://www.kaggle.com/datasets/ealaxi/paysim1

The standard CSV contains 6,362,620 rows and 11 columns. The dashboard screenshot supplied with this project shows an analyzed sample of 250,000 rows. The dataset is synthetic; `isFraud = 1` means the simulator labelled a transaction as fraudulent, not that a real-world crime was verified.

The CSV is large (about 494 MB on Kaggle). To avoid the Shiny browser upload limit, put the CSV beside `app.R` and load it directly using the filename expected by the application. Do not commit the large dataset to GitHub; download it from Kaggle separately.

## Packages

The project uses the packages required by the implementation, including:

- `shiny` for the interactive dashboard
- `tidyverse` / `dplyr` for data preparation and analysis
- `ranger` for Random Forest modelling
- `scales` for readable plot labels
- `lime` for local model explanations
- `ggplot2` (included in tidyverse) for charts

Install any missing packages in R:

```r
install.packages(c("shiny", "tidyverse", "ranger", "scales", "lime"))
```

## Run the dashboard

1. Install R and preferably RStudio.
2. Download the PaySim CSV from Kaggle and extract it.
3. Place the CSV in the same folder as `app.R` using the filename expected by the code.
4. Open the project folder in RStudio.
5. Run:

```r
shiny::runApp()
```

If the app uses a different expected filename, update the filename in `app.R` or rename the CSV to match it.

## Results shown in the supplied dashboard screenshots

These values are transcribed from the supplied screenshots and describe that run only.

| Item | Displayed result |
|---|---:|
| Transactions analyzed | 250,000 |
| Fraud-labelled cases | 8,197 |
| Fraud rate | 3.28% |
| Fraud amount | $11,906,415,428 |
| Accuracy | 99.98% |
| Precision | 99.88% |
| Recall | 99.57% |
| F1-score | 99.73% |
| Confusion matrix: actual legitimate → predicted legitimate | 48,359 |
| Confusion matrix: actual legitimate → predicted fraud | 2 |
| Confusion matrix: actual fraud → predicted legitimate | 7 |
| Confusion matrix: actual fraud → predicted fraud | 1,633 |

The example transaction in the screenshot was flagged for review with an estimated fraud probability of **95.28%** and a threshold of **0.30**.

### LIME example

The screenshot's local explanation displays `emptied = 1` as the strongest supporting condition (weight +0.82), with `type = TRANSFER` contributing +0.04. The displayed explanation fit is **0.16**, which is low; interpret this explanation cautiously. LIME weights are local surrogate weights, not causal proof.

## Interpretation and limitations

- Accuracy alone can be misleading for imbalanced fraud data; precision, recall, F1-score and the confusion matrix are also important.
- PaySim is synthetic, so results may not generalize to real financial transactions.
- The dataset provider cautions that balance columns may be unsuitable in some fraud-detection setups because fraud transactions are cancelled. This project uses balance-derived features (such as `emptied` and `errorBalanceOrig`), so possible dataset-specific shortcuts or label leakage must be considered.
- A flagged transaction is a review recommendation, not proof of fraud or criminal intent.
- The reported metrics depend on the exact sample, train/test split, feature engineering, and threshold. Re-run the dashboard before using results in a formal submission.

## Suggested repository structure

```text
explainable-financial-fraud-detection/
├── app.R
├── README.md
├── report.md
└── Explainable_Financial_Fraud_Detection_Report.docx
```

Keep the dataset out of the repository if it exceeds GitHub's file limits; provide the Kaggle source link instead.

## References

- PaySim dataset on Kaggle: https://www.kaggle.com/datasets/ealaxi/paysim1
- R: https://www.r-project.org/
- Shiny: https://shiny.posit.co/
- ranger: https://cran.r-project.org/package=ranger
- lime: https://cran.r-project.org/package=lime
