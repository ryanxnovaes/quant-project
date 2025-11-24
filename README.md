# ITUB4 Forecasting Project  
**Technical Indicators, Financial EDA, and Rolling k-Nearest Neighbors Forecasting (2010–Present)**

[![R >= 4.3](https://img.shields.io/badge/R-%3E%3D%204.3-276DC3.svg?logo=r&logoColor=white)](https://www.r-project.org/)  
[![Status: Active Development](https://img.shields.io/badge/Status-active--development-blue.svg)](#)  
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This repository contains a complete quantitative modeling pipeline for **forecasting daily closing prices of ITUB4 (Itaú Unibanco)** based on:

1. **High-quality technical indicator engineering** (trend, momentum, volatility, and volume-based indicators)  
2. **Comprehensive exploratory financial analysis (EDA)**  
3. **Rolling-window, out-of-sample machine learning forecasting** using **k-Nearest Neighbors regression (kNN)**  
4. **Grid search over horizons, training windows, and k-values**  
5. **Permutation importance for model interpretability**  
6. **Comparison against a Random Walk benchmark**  

The project was developed in collaboration with **Nicolle Rye Fukushima dos Santos**.

---

## 1. Repository Structure

```
quant-project/
├── codes/
│   ├── codes.Rproj                     # Project environment
│   ├── fetch-indicators.R              # Data acquisition + technical indicator engineering
│   ├── technical_eda.R                 # Financial exploratory data analysis (EDA)
│   ├── knn_rolling_forecast.R          # Rolling-window kNN forecasting + grid search + diagnostics
│
├── data/
│   ├── bronze/
│   │   └── itub4_bronze.parquet        # Raw OHLCV series (Yahoo Finance)
│   └── silver/
│       └── itub4_silver.parquet        # Cleaned dataset with indicators
│
├── figures/
│   ├── eda/                            # All EDA figures
│   └── forecast/                       # Rolling forecasts and diagnostics
│
└── README.md
```

---

## 2. Project Overview

This research aims to model and forecast **daily ITUB4 closing prices** over a 15-year period (2010–present) using a **nonparametric machine learning approach** under a **rolling-window, out-of-sample learning scheme**.

The modeling philosophy is:

- **Empirical**: rely on actual indicator behavior rather than theoretical assumptions.  
- **Dynamic**: recalibrate at each time step using only past data.  
- **Transparent**: evaluate interpretability via **permutation importance**.  
- **Benchmark-driven**: compare against a **Random Walk forecast**.

The pipeline is fully deterministic, reproducible and suited for research, academic reporting and further extension into algorithmic trading frameworks.

---

## 3. Data Source

Daily historical price data (OHLCV) for **ITUB4.SA** were obtained from:

- **Yahoo Finance API** via `quantmod::getSymbols`

**Period:** 2010-01-01 → today  
**Frequency:** daily  

Raw data are saved as:

```
data/bronze/itub4_bronze.parquet
```

---

## 4. Technical Indicators Engineered

The script `fetch-indicators.R` constructs a **broad panel of technical indicators** commonly used in quantitative finance, including:

### Trend Indicators
- **SMA(5)**, **SMA(20)**
- **EMA(5)**
- **WMA(5)**, **WMA(20)**

### Momentum Indicators
- **Momentum (10)**
- **ROC(14)** (Rate of Change)
- **RSI(14)** and **RSI(20)**
- **Stochastic %K (14-3-3)**

### Volume-Based Indicators
- **OBV** — On Balance Volume  
- **MFI(14)** — Money Flow Index

### Trend Strength Measures
- **ADX(14)**, **+DI(14)**, **−DI(14)**

### Volatility Indicators
- **Bollinger Band Width (20, 2σ)**  
- **Rolling Volatility (20-day)**

### Additional Market Variables
- Daily **log-returns**  
- Daily **price range**  
- Monthly seasonal factor (Jan–Dec)

All indicators are merged into a unified, cleaned dataset saved as:

```
data/silver/itub4_silver.parquet
```

---

## 5. Financial Exploratory Data Analysis (EDA)

The script `technical_eda.R` performs a complete financial EDA including:

### 5.1 Price and Trend Analysis
- Long-term closing price evolution  
- LOESS smoothing  
- Monthly price seasonality  

### 5.2 Return and Volatility Analysis
- Daily log-return series  
- Return distribution (with KDE and normal overlay)
- Rolling volatility (20-day)  
- Monthly return distribution  

### 5.3 Technical Indicator Dynamics
- Moving averages panel  
- Oscillators and momentum indicators  
- ADX, DI+, DI− trend strength panels  
- Bollinger Bands width  
- Volatility indicators  

### 5.4 Dependence Structure
- Spearman correlation heatmap  
- Return vs. RSI(14) non-linear relationship  
- ACF and PACF of closing price  

All figures are automatically exported to:

```
figures/eda/
```

---

## 6. Modeling Framework: Rolling kNN Forecasting

The script `knn_rolling_forecast.R` implements the modeling pipeline.

### 6.1 Predictive Target
For each forecasting horizon **h**, the model predicts:

\[
\hat{y}_{t+h} = \text{KNN}(X_t, X_{t-1}, \dots)
\]

### 6.2 Predictor Set (post-VIF selection)

- Lagged close (`Close_lag01`)
- Volume  
- Log-return  
- Price range  
- Rolling volatility  
- ROC(14)  
- Momentum(10)  
- Stochastic-K  
- MACD histogram  
- OBV  
- MFI(14)  
- +DI(14), –DI(14), ADX(14)  
- Bollinger Band width  

### 6.3 Grid Search: Horizons × Training Windows × k

Forecasting horizons:

```
[1, 5, 14, 20, 60]
```

Training windows (rolling):

```
[60, 126, 252, 504]
```

k-values evaluated:

```
1 through 9
```

At each iteration:

- Standardize features **inside** the window  
- Fit kNN on the last `win` observations  
- Predict for the next day  
- Slide window and repeat  

This generates a fully **out-of-sample walk-forward forecast**.

---

## 7. Evaluation Metrics

For every model configuration:

- **MSE**  
- **RMSE**  
- **MAE**  
- **MAPE**  
- **SMAPE**

The **best model** is chosen by the **lowest MSE**.

---

## 8. Best Model and Forecast Visualization

The pipeline outputs:

- Selected **h\*** (best horizon)  
- Selected **window\*** (optimal training window)  
- Selected **k\***  
- Combined forecast vs. observed plot  
- Forecast error structure  

Plots are saved in:

```
figures/forecast/
```

---

## 9. Residual Diagnostics

The following diagnostics are produced:

- Residual time series  
- Residual histogram  
- Cumulative absolute error (CAE)  

These tools verify temporal dependence, volatility clustering and error distribution.

---

## 10. Permutation Importance (Model Interpretability)

Permutation importance quantifies the effect of each predictor on model error:

\[
\text{Importance}(X_j)
= \text{MSE}_{\text{perm}(X_j)} - \text{MSE}_{\text{base}}
\]

Steps:

1. Permute a single predictor  
2. Re-run full rolling kNN forecasting  
3. Measure ΔMSE  

A ranked ΔMSE plot is produced showing the most influential predictors.

---

## 11. Benchmark: Random Walk Forecasting

The baseline forecast assumes:

\[
\hat{y}_{t+h} = y_t
\]

Evaluated at the same horizons:

```
[1, 5, 14, 20, 60]
```

Metrics (MSE, RMSE, MAE, MAPE, SMAPE) allow a **direct performance comparison** between:

- **Rolling kNN model**  
- **Naive Random Walk model**  

This ensures methodological rigor in financial forecasting assessment.

---

## 12. How to Reproduce

Clone the repository:

```bash
git clone https://github.com/ryanxnovaes/quant-project.git
```

Open in RStudio:

```r
open "codes/codes.Rproj"
```

Run:

```r
source("codes/fetch-indicators.R")
source("codes/technical_eda.R")
source("codes/knn_rolling_forecast.R")
```

Figures are exported automatically.

---

## 13. Citation

If you use this repository, please cite:

**Novaes Pereira, R., & Fukushima dos Santos, N. R. (2025).**  
*ITUB4 Price Forecasting Using Technical Indicators and Rolling kNN Models.*  
GitHub: https://github.com/ryanxnovaes/quant-project

---

## 14. Contact

**Ryan Novaes Pereira**  
Email: ryan.novaes@unesp.br  

**Nicolle Rye Fukushima dos Santos**  
Email: nicolle.rye@unesp.br
