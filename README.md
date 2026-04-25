# 🏦 Credit Risk Analytics & Business Intelligence — Canadian Banking Portfolio

![R](https://img.shields.io/badge/Language-R-276DC3?style=flat&logo=r)
![Status](https://img.shields.io/badge/Status-Completed-brightgreen)
![Domain](https://img.shields.io/badge/Domain-Credit%20Risk-red)

## 📌 Project Overview

This project simulates a **Canadian retail banking loan portfolio** to perform credit risk segmentation, default probability modeling, and portfolio-level business intelligence reporting. It applies industry-standard risk analytics concepts including **Probability of Default (PD)**, **Loss Given Default (LGD)**, and **Expected Loss (EL)**.

---

## 🎯 Business Objectives

- Segment loan portfolios by default risk level
- Estimate probability of default (PD) using predictive modeling
- Develop KPI dashboards for portfolio monitoring
- Produce decision-support outputs for credit analysts and risk managers

---

## 🗂️ Dataset Description

| Variable | Description |
|---|---|
| `loan_id` | Unique loan identifier |
| `credit_score` | Borrower credit score (300–900) |
| `loan_amount` | Loan value in CAD |
| `income` | Annual borrower income |
| `debt_to_income` | Debt-to-income ratio |
| `loan_term_months` | Loan duration |
| `employment_status` | Employed / Self-employed / Unemployed |
| `num_missed_payments` | Historical payment defaults |
| `default` | Binary: 1 = defaulted, 0 = performing |

> ⚠️ *All data is simulated. No real client data was used.*

---

## 🔧 Methods & Tools

### Risk Modeling
- **Logistic Regression** — PD estimation (Basel-aligned approach)
- **Random Forest** — Non-linear risk classification
- **Scorecard Development** — WoE (Weight of Evidence) transformation

### Portfolio Analytics
- **Risk Segmentation** — Low / Medium / High / Very High buckets
- **Expected Loss Calculation** — EL = PD × LGD × EAD
- **Vintage Analysis** — Default rate tracking by loan cohort

### Visualization & BI
- **Power BI-style dashboards** built in R (ggplot2 + Shiny)
- KPI cards: Total Exposure, Default Rate, Average PD, NPL Ratio

### Tools
```
R | tidyverse | caret | scorecard | ggplot2 | Shiny | dplyr | knitr
```

---

## 📊 Key Results

### Portfolio Summary (Simulated — 5,000 loans)

| Metric | Value |
|---|---|
| Total Portfolio Value | CAD 125M |
| Overall Default Rate | 8.3% |
| Average Probability of Default | 11.2% |
| Non-Performing Loan (NPL) Ratio | 7.6% |
| Expected Loss (EL) | CAD 3.2M |

### Model Performance

| Model | Accuracy | Gini Coefficient | AUC |
|---|---|---|---|
| Logistic Regression | 79.2% | 0.58 | 0.79 |
| Random Forest | 85.4% | 0.70 | 0.85 |

---

## 📁 Project Structure

```
credit-risk-canadian-banking/
│
├── data/
│   └── simulated_loan_portfolio.csv
├── scripts/
│   ├── 01_data_simulation.R
│   ├── 02_eda_and_segmentation.R
│   ├── 03_pd_modeling.R
│   ├── 04_expected_loss.R
│   └── 05_dashboard.R
├── outputs/
│   ├── risk_segments_plot.png
│   ├── roc_curve.png
│   ├── portfolio_summary.png
│   └── scorecard_table.csv
└── README.md
```

---

## 💡 Key Insights

1. **Debt-to-income ratio** is the strongest predictor of default (highest feature importance)
2. **Credit scores below 580** show a 3x higher default rate than the portfolio average
3. **Self-employed borrowers** exhibit 40% higher PD controlling for income level
4. Vintage analysis shows **loans originated in Q4 underperform** Q2 cohorts by 15%

---

## 📚 Concepts Applied

- Basel II/III PD framework
- Weight of Evidence (WoE) & Information Value (IV)
- Scorecard calibration
- Expected Loss decomposition (PD × LGD × EAD)
- IFRS 9 staging logic (Stage 1 / 2 / 3)

---

## 👤 Author

**Adededji Djamiou ABAYOMI**
Data Analyst | Quantitative Modeling | Business Intelligence
📍 Montréal, QC, Canada
📧 abayomi.adededji.djamiou@gmail.com
🔗 [LinkedIn](https://linkedin.com)
