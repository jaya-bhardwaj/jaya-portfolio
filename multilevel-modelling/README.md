# Multilevel Modelling

## Overview

This project applies multilevel modelling to two datasets to investigate hierarchically structured data. Section 1 analyses 2022 Canadian PISA data to identify school and student-level predictors of mathematical ability, using a two-level random slope model with cross-level interactions. Section 2 analyses British Household Panel Survey (BHPS) data to model how attitudes towards women and family evolve with age from 20 to 36, using a longitudinal growth curve framework. 

## Tech Stack

R, lme4, nlme, dplyr, ggplot2, tidyr, lattice, performance

## Key Results

**Section 1 — Canadian PISA Maths (Two-Level Random Slope Model)**

| Finding | Detail |
|---|---|
| ICC (null model) | 17.0% of variance attributable to between-school differences |
| Strongest predictor | Family wealth (`homepos`): +0.256 SD per unit increase |
| Gender effect | Girls score 0.133 SD lower than boys on average |
| Private school effect | +0.382 SD compared to government schools |
| Cross-level interaction | Wealth effect significantly weaker in private schools (reduced to 0.096 SD) |
| Unexplained school variance | 76% remains unexplained; teaching quality and curriculum likely play a role |

- Random slopes retained for `homepos`, `female`, and `hisced` — all significant via LR test
- The gender gap varies across schools; in some schools girls outperform boys
- Model selection via backward elimination at each stage, with all LR tests conducted under ML

**Section 2 — BHPS Attitudes Growth Curve (Two-Level Random Slope Model)**

| Finding | Detail |
|---|---|
| ICC (null model) | 56.4% of variance attributable to stable between-individual differences |
| Age effect | Attitudes decline by 0.012 SD per year on average |
| Gender effect | Females score 0.310 SD higher (more liberal) than males |
| Partner effect | Having a coresident partner associated with −0.110 SD |
| Intercept–slope correlation | −0.497: more liberal at 20 → steeper decline with age |
| AR(1) residuals | Not significant (p = 0.9996); independent residuals retained |

- Between-individual variance follows a U-shape with age, lowest around age 26 and highest at 36
- Quadratic age term non-significant; linear random slope model retained
- Country and highest qualification dropped via backward elimination

## Files

- `multilevel_modelling.R` 
- `pisaCanadaMaths.csv`
- `attfamUK.csv`

## Methodology Notes

**Two-level structure (S1)** — students (level 1) nested within schools (level 2). Random slopes fitted for `homepos`, `female`, and `hisced`; a cross-level interaction between `homepos` and school type retained in the final model.

**Grand-mean centring (S1)** — all continuous level 1 predictors centred at their grand mean; `stubeha` centred at the school-level mean. Ensures the intercept is interpretable as the expected score for an average student.

**Longitudinal structure (S2)** — repeated occasions (level 1) nested within individuals (level 2). Age centred at 20 (entry age) and scaled by dividing by 8 to resolve convergence issues in the random slope model.

**Model selection** — backward elimination with LR tests under ML at each stage. Test set evaluated once at the end; no data-driven decisions made on test data.

**Diagnostics** — level 1 and level 2 residuals inspected via histograms, QQ plots, and caterpillar plots. Level 2 residuals show heavier tails than expected in both sections, likely due to small cluster sizes.
