# S&P 500 Return Prediction - Rolling OLS & Principal Component Regression

## Overview

This project investigates whether next-day S&P 500 returns can be predicted using lagged index returns and the contemporaneous returns of 10 large-cap stocks. Using five years of daily data (2020–2025), we implement a rolling-window prediction strategy with exponential smoothing volatility normalisation, ordinary least squares regression, and principal component regression (PCR). The trading strategy takes a long or short position in the S&P 500 based on the sign of the predicted return, evaluated via annualised Sharpe ratio. PCR with one factor and a window of D = 250 emerges as the best-performing model, delivering more stable and consistently positive out-of-sample Sharpe ratios than plain OLS across both validation and test sets.

## Tech Stack

R, quantmod

## Stocks

| Ticker | Company |
|---|---|
| ^GSPC | S&P 500 Index |
| AAPL | Apple Inc. |
| AMZN | Amazon.com, Inc. |
| AVGO | Broadcom Inc. |
| GOOGL | Alphabet Inc. (Class A) |
| IBM | IBM Corporation |
| META | Meta Platforms, Inc. |
| MSFT | Microsoft Corporation |
| NVDA | NVIDIA Corporation |
| PLTR | Palantir Technologies Inc. |
| TSLA | Tesla, Inc. |

## Key Results

**OLS (q = 0 and q = 1, optimal D = 190)**

| Set | Sharpe Ratio |
|---|---|
| Training | Negative across all D |
| Validation | Positive at D = 150–200 |
| Test | Positive at D = 150–200 |

- Training condition numbers rise to ~60, indicating multicollinearity in the full covariate set
- Validation and test condition numbers fall to 14–28, where OLS is more numerically reliable
- Adding one lag of the S&P 500 (q = 1) adds negligible predictive power over q = 0

**PCR (optimal: D = 250, k = 1 factor)**

| Set | vs OLS |
|---|---|
| Training | Higher Sharpe across nearly all D |
| Validation | Strictly above OLS for most D; several values above 1 |
| Test | Avoids large negative dips; more stable than OLS |

- Scree plots confirm the first principal component captures most covariate variance
- PCR with k = 2 is marginally smoother than k = 1 but both dominate OLS

## Files

- `Stock return prediction.r`

## Methodology Notes

**Data split** — 50% training, 25% validation, 25% test using a strict walk-forward split with no lookahead.

**Volatility normalisation** — each return series is de-volatilised using exponential smoothing with asset-specific λ estimated by MLE on the training set. The same λ values are applied to validation and test sets.

**Rolling OLS** — at each time step t, OLS is fitted on the most recent D de-volatilised returns. The predicted sign determines the long/short position; Sharpe ratio is annualised using √250.

**PCR** — PCA is applied to the covariate window at each step; the first k principal components replace the raw stock returns as regressors, reducing dimensionality and multicollinearity.

**Tuning** — window length D and number of PCR factors k are selected by maximising the validation Sharpe ratio; the test set is only evaluated once at the end.
