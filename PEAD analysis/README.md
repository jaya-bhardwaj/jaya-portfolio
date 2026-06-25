# Project Name

## Overview
Post-earnings announcement drift (PEAD) is the well-documented tendency of stock prices to continue drifting in the direction of an earnings surprise for weeks after an announcement - a persistent challenge to the efficient market hypothesis. This project investigates whether PEAD still exists in the post-COVID S&P 500 (2021–2024) and whether machine learning can exploit it more effectively than the classical SUE-sorting approach. Using a strict walk-forward framework (training on 2012–2020, testing on 2021–2024) across 20,341 earnings announcements, we find that PEAD persists but has compressed into shorter windows, and that a regularised elastic net logistic regression - not a neural network - delivers the strongest out-of-sample long-short portfolio spread.

## Tech Stack
Python, PyTorch, Optuna, Data sources 

## Key Results
| Model | Long-Short Spread (CAR 2–60) | t-stat | p-value |
|---|---|---|---|
| Logistic Regression | 0.66% | 1.43 | 0.153 |
| **Elastic Net Logistic** | **2.34%** | **5.18** | **<0.001** |
| Neural Network (ensemble of 5) | 1.26% | 2.63 | 0.009 |

- PEAD persists at the 20-day horizon (top-bottom SUE spread: t = 2.32, p = 0.021) but is not significant at 60 days (p = 0.160), consistent with faster price discovery in large-cap stocks post-COVID
- Elastic net selects 5 of 11 predictors — `McapRank`, `TurnRank`, `VolatilityRank`, `swbeta`, `suescore` — zeroing all macroeconomic controls
- The 2.34% spread compares favourably with Meursault et al. (2023), who report 1.54% using classic numerical SUE features; our model falls between their numerical and text-based benchmarks
- Neural network adds predictive value but does not outperform the elastic net, consistent with the limited training sample size (~13K observations)

## Files
- `PEAD analysis.ipynb`
- - `ibes_df_final.csv` 
