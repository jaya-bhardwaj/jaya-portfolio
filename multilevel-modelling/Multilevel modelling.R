# Question 1
rm(list = ls())

##############################################################
# Load packages
##############################################################

library(dplyr)      
library(lme4)       
library(lattice)    
library(ggplot2)    
library(performance) 
library(tidyr)      

##############################################################
# Read data
##############################################################

pisa <- read.csv(file = "OneDrive/Year 3/ST314/Individual project/pisaCanadaMaths.csv")

##############################################################
# Descriptive analysis
##############################################################

head(pisa)
str(pisa)

##############################################################
# Recode categorical variables as factors
pisa$immig <- factor(pisa$immig,
                     levels = c(1, 2, 3),
                     labels = c("Native", "SecondGen", "FirstGen"))

pisa$langn <- factor(pisa$langn,
                     levels = c(1, 2, 3),
                     labels = c("English", "French", "Other"))

pisa$schprivate <- factor(pisa$schprivate,
                          levels = c(0, 1),
                          labels = c("Government", "Private"))

##############################################################
# Check for missing data
colSums(is.na(pisa))

##############################################################
# Total students
nrow(pisa)     
# Total schools
length(unique(pisa$schoolid))     

##############################################################
# Create school-level dataset
pisa.s <- pisa %>%
  group_by(schoolid) %>%
  summarise(
    schoolid   = first(schoolid),
    mean.zmath = mean(zmath, na.rm = TRUE),
    nstud      = n(),
    schprivate = first(schprivate),
    stubeha    = first(stubeha)
  )

summary(pisa.s$nstud)   
head(pisa.s)

##############################################################
# Univariate summaries
# Level 1 continuous
summary(pisa$zmath)
sd(pisa$zmath)    

summary(pisa$age)
sd(pisa$age)

summary(pisa$hisced)
sd(pisa$hisced)

summary(pisa$homepos)
sd(pisa$homepos)

# Level 2 continuous — school level only
summary(pisa.s$stubeha)
sd(pisa.s$stubeha)

# Level 1 categorical proportions
round(prop.table(table(pisa$female)) * 100, 1)
round(prop.table(table(pisa$immig))  * 100, 1)
round(prop.table(table(pisa$langn))  * 100, 1)

# Level 2 categorical — school level only
round(prop.table(table(pisa.s$schprivate)) * 100, 1)

##############################################################
# Histograms
hist(pisa$zmath,   main = "Histogram of zmath",   xlab = "Standardised maths score")
hist(pisa$age,     main = "Histogram of age",      xlab = "Age")
hist(pisa$hisced,  main = "Histogram of hisced",   xlab = "Highest parental education")
hist(pisa$homepos, main = "Histogram of homepos",  xlab = "Home possessions")

hist(pisa.s$stubeha, main = "Histogram of stubeha (school level)",
     xlab = "Student behaviour hindering learning")

##############################################################
# Boxplots
boxplot(zmath ~ female, data = pisa,
        main = "Maths score by gender",
        ylab = "zmath", col = c("lightblue", "pink"))

boxplot(zmath ~ immig, data = pisa,
        main = "Maths score by immigration background",
        ylab = "zmath", col = "lightgreen")

boxplot(zmath ~ langn, data = pisa,
        main = "Maths score by language at home",
        ylab = "zmath", col = "lightyellow")

boxplot(mean.zmath ~ schprivate, data = pisa.s,
        main = "School mean maths score by school type",
        ylab = "School mean zmath", col = c("lightblue", "lightyellow"))

##############################################################
# Scatterplots
plot(pisa$age, pisa$zmath, pch = ".", col = rgb(0, 0, 0, 0.15),
     xlab = "Age", ylab = "zmath", main = "Maths score vs age")
lines(lowess(pisa$age, pisa$zmath),       col = "red",  lwd = 2)
abline(lm(zmath ~ age, data = pisa),      col = "blue", lwd = 1, lty = 2)

plot(pisa$hisced, pisa$zmath, pch = ".", col = rgb(0, 0, 0, 0.15),
     xlab = "Parental education (hisced)", ylab = "zmath",
     main = "Maths score vs parental education")
lines(lowess(pisa$hisced, pisa$zmath),    col = "red",  lwd = 2)
abline(lm(zmath ~ hisced, data = pisa),   col = "blue", lwd = 1, lty = 2)

plot(pisa$homepos, pisa$zmath, pch = ".", col = rgb(0, 0, 0, 0.15),
     xlab = "Home possessions (homepos)", ylab = "zmath",
     main = "Maths score vs family wealth")
lines(lowess(pisa$homepos, pisa$zmath),   col = "red",  lwd = 2)
abline(lm(zmath ~ homepos, data = pisa),  col = "blue", lwd = 1, lty = 2)

##############################################################
# Bivariate plots
plot(pisa.s$stubeha, pisa.s$mean.zmath,
     pch = 16, col = "steelblue",
     xlab = "Student behaviour hindering learning (stubeha)",
     ylab = "School mean zmath",
     main = "School mean zmath vs student behaviour score")
lines(lowess(pisa.s$stubeha, pisa.s$mean.zmath), col = "red", lwd = 2)

##############################################################
# Caterpillar plot of ranked school means (EDA version)
within_sd <- sd(residuals(lm(zmath ~ 1, data = pisa)))
pisa.s <- pisa.s %>%
  arrange(mean.zmath) %>%
  mutate(rank = row_number(),
         se   = within_sd / sqrt(nstud))

plot(pisa.s$rank, pisa.s$mean.zmath,
     pch = 16, cex = 0.7, col = "blue",
     xlab = "Rank of school",
     ylab = "School mean zmath",
     main = "Ranked school mean maths scores with approximate 95% CI")

segments(pisa.s$rank,
         pisa.s$mean.zmath - 1.96 * pisa.s$se,
         pisa.s$rank,
         pisa.s$mean.zmath + 1.96 * pisa.s$se,
         col = "black")

points(pisa.s$rank, pisa.s$mean.zmath,
       pch = 16, cex = 0.7, col = "blue")

abline(h = mean(pisa$zmath), lty = 2, col = "red")

##############################################################
# Within-school regression panels
set.seed(123)
sample_schools <- sample(unique(pisa$schoolid), 16)
pisa.sample    <- pisa[pisa$schoolid %in% sample_schools, ]

xyplot(zmath ~ hisced | factor(schoolid), data = pisa.sample,
       type = c("p", "r"), col.line = "red", pch = ".", alpha = 0.4,
       xlab = "Parental education (hisced)", ylab = "zmath",
       main = "Within-school regression: zmath ~ hisced (16 schools)")

xyplot(zmath ~ homepos | factor(schoolid), data = pisa.sample,
       type = c("p", "r"), col.line = "blue", pch = ".", alpha = 0.4,
       xlab = "Home possessions (homepos)", ylab = "zmath",
       main = "Within-school regression: zmath ~ homepos (16 schools)")

##############################################################
# Grand-mean centering of continuous predictors
pisa$age.gm     <- pisa$age     - mean(pisa$age,     na.rm = TRUE)
pisa$hisced.gm  <- pisa$hisced  - mean(pisa$hisced,  na.rm = TRUE)
pisa$homepos.gm <- pisa$homepos - mean(pisa$homepos, na.rm = TRUE)

pisa.s$stubeha.gm <- pisa.s$stubeha - mean(pisa.s$stubeha, na.rm = TRUE)
pisa <- left_join(pisa,
                  pisa.s %>% select(schoolid, stubeha.gm),
                  by = "schoolid")

##############################################################
# Step 1: Null model
##############################################################
sl.m <- lm(zmath ~ 1, data = pisa)
summary(sl.m)

vc.re.m <- lmer(zmath ~ (1 | schoolid), data = pisa, REML = FALSE)
anova(vc.re.m, sl.m)
summary(vc.re.m)

##############################################################
performance::icc(vc.re.m)

# Extract variance components for PVR calculation
var.components <- as.data.frame(VarCorr(vc.re.m))
sig2u0 <- var.components$vcov[1] # between-school variance
sig2e  <- var.components$vcov[2] # within-school variance

# 95% plausible values range for school means 
beta0 <- fixef(vc.re.m)[1]
sd.u0 <- sqrt(sig2u0)
cat("95% PVR for school means:",
    round(beta0 - 1.96 * sd.u0, 3), "to",
    round(beta0 + 1.96 * sd.u0, 3), "\n")

# Empirical Bayes caterpillar plot of school effects
uest <- ranef(vc.re.m)
u    <- uest[[1]]
use  <- sqrt(attr(uest[[1]], "postVar")[1, , ])

school       <- rownames(u)
schoolid.num <- 1:length(school)
udata        <- cbind(school, schoolid.num, u, use)
colnames(udata) <- c("school", "schoolid.num", "u0", "u0se")
udata        <- udata[order(udata$u0), ]
udata        <- cbind(udata, 1:nrow(udata))
colnames(udata)[5] <- "rank"

plot(udata$rank, udata$u0, type = "n",
     xlab = "Rank of school effect",
     ylab = "Estimate of school effect (u0)",
     main = "Caterpillar plot of school effects — null model")
segments(udata$rank,
         udata$u0 - 1.96 * udata$u0se,
         udata$rank,
         udata$u0 + 1.96 * udata$u0se)
points(udata$rank, udata$u0, col = "blue")
abline(h = 0, col = "red")

##############################################################
# Step 2: Level 1 main effects 
##############################################################

mri1 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
               (1 | schoolid),
             data = pisa, REML = FALSE)
anova(mri1, vc.re.m) # Significant
summary(mri1)$coefficients

# Remove langn (|t| values 4.799, 2.220)
mri1.1 <- lmer(zmath ~ age.gm + female + immig + hisced.gm + homepos.gm +
                 (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri1.1, mri1) # Significant
summary(mri1.1)$coefficients

mri1.final <- mri1

##############################################################
# Step 3: Level 1 interactions 
##############################################################

mri2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
               female:age.gm + female:immig + female:langn + female:hisced.gm + female:homepos.gm +
               (1 | schoolid),
             data = pisa, REML = FALSE)
anova(mri2, mri1.final) # borderline 
summary(mri2)$coefficients

# Remove female:age.gm first (|t| = 0.206)
mri2.1 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
                 female:immig + female:langn + female:hisced.gm + female:homepos.gm +
                 (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri2.1, mri2) # Insignificant
summary(mri2.1)$coefficients

# Remove female:langn (|t| values 0.437, 0.265)
mri2.2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
                 female:immig + female:hisced.gm + female:homepos.gm +
                 (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri2.2, mri2.1) # Insignificant
summary(mri2.2)$coefficients

# Remove female:immig (|t| values -0.904, -1.467)
mri2.3 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
                 female:hisced.gm + female:homepos.gm +
                 (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri2.3, mri2.2) # Insignificant
summary(mri2.3)$coefficients

# Remove female:homepos.gm (|t| = 1.147)
mri2.4 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
                 female:hisced.gm +
                 (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri2.4, mri2.3) # Insignificant
summary(mri2.4)$coefficients

# Remove female:hisced.gm (|t| = 2.878)
mri2.5 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm +
                 (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri2.5, mri2.4) # Significant
summary(mri2.5)$coefficients

anova(mri2.4, mri1.final) # Significant

mri2.final <- mri2.4

##############################################################
# Step 4: Level 2 covariates
##############################################################

mri3.1 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm + (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri3.1, mri2.final) # Significant
summary(mri3.1)$coefficients

# Remove schprivate (|t| = 5.596)
mri3.2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 stubeha.gm + (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri3.2, mri3.1) # Significant
summary(mri3.2)$coefficients

# Remove stubeha.gm (|t| = 6.222)
mri3.3 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + (1 | schoolid),
               data = pisa, REML = FALSE)
anova(mri3.3, mri3.1) # Significant
summary(mri3.3)$coefficients

mri3.final <- mri3.1

##############################################################
# Step 5: Random slopes
##############################################################

# Test random slope for hisced.gm 
rs.hisced <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                    female:hisced.gm +
                    schprivate + stubeha.gm +
                    (1 + hisced.gm | schoolid),
                  data = pisa, REML = FALSE)
anova(rs.hisced, mri3.final) # Significant
summary(rs.hisced)$coefficients

# Test random slope for female 
rs.female <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                    female:hisced.gm +
                    schprivate + stubeha.gm +
                    (1 + female | schoolid),
                  data = pisa, REML = FALSE)
anova(rs.female, mri3.final) # Significant
summary(rs.female)$coefficients

# Test random slope for homepos.gm 
rs.homepos <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                     female:hisced.gm +
                     schprivate + stubeha.gm +
                     (1 + homepos.gm | schoolid),
                   data = pisa, REML = FALSE)
anova(rs.homepos, mri3.final) # Significant
summary(rs.homepos)$coefficients

# Test random slope for age.gm 
rs.age <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 (1 + age.gm | schoolid),
               data = pisa, REML = FALSE)
anova(rs.age, mri3.final) # Borderline
summary(rs.age)$coefficients

rs1 <- rs.hisced

##############################################################
# Build combined random slope model
# Add female random slope
rs2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
              female:hisced.gm +
              schprivate + stubeha.gm +
              (1 + hisced.gm + female | schoolid),
            data = pisa, REML = FALSE)
anova(rs2, rs1) # Significant
summary(rs2)$coefficients

# Add homepos.gm random slope
rs3 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
              female:hisced.gm +
              schprivate + stubeha.gm +
              (1 + hisced.gm + female + homepos.gm | schoolid),
            data = pisa, REML = FALSE)
anova(rs3, rs2) # Significant
summary(rs3)$coefficients

rs.final <- rs3

##############################################################
# Step 6: Cross-Level Interactions
##############################################################
cli1.1 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 hisced.gm:schprivate +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli1.1, rs.final) # Insignificant
summary(cli1.1)$coefficients

cli1.2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 hisced.gm:stubeha.gm +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli1.2, rs.final) # Insignificant
summary(cli1.2)$coefficients

cli2.1 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 female:schprivate +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli2.1, rs.final) # Insignificant
summary(cli2.1)$coefficients

cli2.2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 female:stubeha.gm +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli2.2, rs.final) # Insignificant
summary(cli2.2)$coefficients

cli3.1 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 homepos.gm:schprivate +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli3.1, rs.final) # Significant
summary(cli3.1)$coefficients

cli3.2 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 homepos.gm:stubeha.gm +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli3.2, rs.final) # Significant
summary(cli3.2)$coefficients

cli3.3 <- lmer(zmath ~ age.gm + female + immig + langn + hisced.gm + homepos.gm + 
                 female:hisced.gm +
                 schprivate + stubeha.gm +
                 homepos.gm:schprivate + homepos.gm:stubeha.gm +
                 (1 + hisced.gm + female + homepos.gm | schoolid),
               data = pisa, REML = FALSE)
anova(cli3.3, rs.final) # Significant
anova(cli3.1, cli3.3) # Insignificant
anova(cli3.2, cli3.3) # Significant

final.model <- cli3.1
summary(final.model)$coefficients

############################################################
# Final model diagnostics
############################################################
sigma.final <- as.data.frame(VarCorr(final.model), comp = "Variance")
sigma.final

beta0.final        <- fixef(final.model)[1]
beta.hisced.final  <- fixef(final.model)["hisced.gm"]
beta.female.final  <- fixef(final.model)["female"]
beta.homepos.final <- fixef(final.model)["homepos.gm"]

cat("95% PVR intercepts:   ",
    round(beta0.final + c(-1,1)*1.96*sqrt(sigma.final$vcov[1]), 3), "\n")
cat("95% PVR hisced slope: ",
    round(beta.hisced.final + c(-1,1)*1.96*sqrt(sigma.final$vcov[2]), 3), "\n")
cat("95% PVR female slope: ",
    round(beta.female.final + c(-1,1)*1.96*sqrt(sigma.final$vcov[3]), 3), "\n")
cat("95% PVR homepos slope (govt schools): ",
    round(beta.homepos.final + c(-1,1)*1.96*sqrt(sigma.final$vcov[4]), 3), "\n")

# Reduction in homepos slope variance from rs3 to final model
sigma.rs3 <- as.data.frame(VarCorr(rs3), comp = "Variance")
cat("homepos slope variance in rs3:         ",
    round(sigma.rs3$vcov[4], 4), "\n")
cat("homepos slope variance in final model: ",
    round(sigma.final$vcov[4], 4), "\n")
cat("Proportion explained:                  ",
    round((sigma.rs3$vcov[4] - sigma.final$vcov[4]) /
            sigma.rs3$vcov[4], 3), "\n")

# VPC of final model
sig2u0.final <- sigma.final$vcov[1]
sig2e.final  <- sigma.final$vcov[11]
vpc.final    <- sig2u0.final / (sig2u0.final + sig2e.final)
cat("VPC final model:", round(vpc.final, 3), "\n")

# Cumulative R2 from null to final model
sig2u0.null <- as.data.frame(VarCorr(vc.re.m), comp = "Variance")$vcov[1]
sig2e.null  <- as.data.frame(VarCorr(vc.re.m), comp = "Variance")$vcov[2]

R2_total_final <- ((sig2u0.null + sig2e.null) - 
                     (sig2u0.final + sig2e.final)) /
  (sig2u0.null + sig2e.null)
R2_1_final     <- (sig2e.null - sig2e.final) / sig2e.null
R2_2_final     <- (sig2u0.null - sig2u0.final) / sig2u0.null

cat("Cumulative R2 total (null to final):", round(R2_total_final, 3), "\n")
cat("Cumulative R2 L1:                   ", round(R2_1_final, 3), "\n")
cat("Cumulative R2 L2:                   ", round(R2_2_final, 3), "\n")

#############################################################
# Level 1 residuals
hist(residuals(final.model),
     xlab = "Residual",
     main = "Histogram of level-1 residuals — final model")

qqnorm(residuals(final.model),
       main = "QQ plot of level-1 residuals — final model")
qqline(residuals(final.model), col = "red")

# Level 2 residuals
# Check how many NaNs were produced
uest.final <- ranef(final.model)
u.final    <- uest.final[[1]]

use.final.raw <- sqrt(attr(uest.final[[1]], "postVar")[1, , ])
cat("Number of NaNs:", sum(is.nan(use.final.raw)), "\n")
cat("Number of negative values before sqrt:", 
    sum(attr(uest.final[[1]], "postVar")[1, , ] < 0), "\n")

use.final.correct <- sqrt(attr(uest.final[[1]], "postVar")[1, 1, ])
cat("Number of NaNs with correct extraction:", 
    sum(is.nan(use.final.correct)), "\n")

# Recompute standardised residuals with correct SE
u.final.st.correct <- u.final[, 1] / use.final.correct

# Check range — should be roughly -3 to 3
summary(u.final.st.correct)

qqnorm(u.final.st.correct,
       main = "QQ plot of school residuals — final model")
qqline(u.final.st.correct, col = "red")

hist(u.final.st.correct,
     xlab = "Standardised school residual",
     main = "Histogram of school residuals — final model")

#############################################################
# Residuals vs fitted for homoskedasticity check
plot(final.model, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = ".",
     xlab = "Fitted values", 
     ylab = "Standardised residual",
     main = "Residuals vs fitted — final model")
#############################################################
# Caterpillar plot of school effects from final model
# Extract posterior variance for intercept only (row 1, col 1, all schools)
use.final  <- sqrt(attr(uest.final[[1]], "postVar")[1, 1, ])

# Standardised school intercept residuals
u.final.st <- u.final[, 1] / use.final

# Caterpillar plot
u.df      <- data.frame(u0 = u.final[, 1], se = use.final)
u.df      <- u.df[order(u.df$u0), ]
u.df$rank <- 1:nrow(u.df)

plot(u.df$rank, u.df$u0, type = "n",
     xlab = "Rank of school effect",
     ylab = "Estimate of school effect (u0)",
     main = "Caterpillar plot of school effects — final model")

segments(u.df$rank,
         u.df$u0 - 1.96 * u.df$se,
         u.df$rank,
         u.df$u0 + 1.96 * u.df$se,
         col = "black")

points(u.df$rank, u.df$u0, col = "blue", pch = 16, cex = 0.5)

abline(h = 0, col = "red", lty = 2)

#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
# Question 2
rm(list = ls())

##############################################################
# Load packages
##############################################################

library(dplyr)      
library(lme4)       
library(lattice)    
library(ggplot2)    
library(performance) 
library(tidyr) 
library(nlme)

##############################################################
# Read data
##############################################################

attfam <- read.csv(file = "OneDrive - Dr Challoner's High School/Year 3/ST314/Individual project/attfamUK.csv")

############################################################
# # Descriptive analysis
############################################################

head(attfam)
str(attfam)

# Number of individuals (level 2) and total observations (level 1)
nrow(attfam)
length(unique(attfam$pid))

# Number of observations per individual
obs.per.ind <- attfam %>% group_by(pid) %>% summarise(n.obs = n())
table(obs.per.ind$n.obs)
summary(obs.per.ind$n.obs)

############################################################
# Recode variables
attfam$female  <- factor(attfam$female,
                         levels = c(0, 1),
                         labels = c("Male", "Female"))

attfam$qualhi  <- factor(attfam$qualhi,
                         levels = c(0, 1),
                         labels = c("A-level_or_lower", "Post_school"))

attfam$partner <- factor(attfam$partner,
                         levels = c(0, 1),
                         labels = c("No_partner", "Partner"))

attfam$country <- factor(attfam$country,
                         levels = c(1, 2, 3, 4),
                         labels = c("England", "Wales", "Scotland", "N_Ireland"))

############################################################
# Missing data
colSums(is.na(attfam))

############################################################
# Univariate summaries
# Response variable
summary(attfam$zattfam)
sd(attfam$zattfam, na.rm = TRUE)
hist(attfam$zattfam,
     main = "Histogram of zattfam",
     xlab = "Attitudes towards women and family (standardised)",
     col  = "steelblue")

summary(attfam$age)
hist(attfam$age,
     main = "Histogram of age",
     xlab = "Age (years)", col = "lightblue")

# Time-varying categorical predictors 
round(prop.table(table(attfam$qualhi))  * 100, 1)
round(prop.table(table(attfam$partner)) * 100, 1)
round(prop.table(table(attfam$country)) * 100, 1)

# Time-invariant categorical predictors
attfam.i <- attfam %>%
  group_by(pid) %>%
  summarise(female  = first(female))

round(prop.table(table(attfam.i$female)) * 100, 1)

############################################################
# Distribution of zattfam by wave
wave.summary <- attfam %>%
  group_by(wave) %>%
  summarise(
    year     = first(year),
    n        = n(),
    mean.att = mean(zattfam,  na.rm = TRUE),
    var.att  = var(zattfam,   na.rm = TRUE),
    mean.age = mean(age,      na.rm = TRUE)
  )
print(wave.summary)

# Plot mean zattfam by wave
plot(wave.summary$wave, wave.summary$mean.att,
     type = "b", pch = 16, col = "steelblue",
     xlab = "Wave", ylab = "Mean zattfam",
     main = "Mean attitudes towards women and family by wave")
abline(h = 0, lty = 2, col = "grey")

# Plot variance by wave
plot(wave.summary$wave, wave.summary$var.att,
     type = "b", pch = 16, col = "salmon",
     xlab = "Wave", ylab = "Variance of zattfam",
     main = "Variance of zattfam by wave")

############################################################
# Within-individual correlation matrix
# Reshape to wide format for correlation matrix
attfam.wide <- attfam %>%
  select(pid, wave, zattfam) %>%
  tidyr::pivot_wider(names_from = wave, values_from = zattfam,
                     names_prefix = "wave")

# Correlation matrix 
cor.matrix <- cor(attfam.wide[, -1], use = "pairwise.complete.obs")
round(cor.matrix, 3)

############################################################
# Individual trajectories sample
set.seed(123)
sample_ids <- sample(unique(attfam$pid), 9)
attfam.sample <- attfam[attfam$pid %in% sample_ids, ]

xyplot(zattfam ~ age | factor(pid), data = attfam.sample,
       type = c("p", "l"), col = "steelblue",
       xlab = "Age (years)", ylab = "zattfam",
       main = "Individual attitude trajectories (sample of 9)")

############################################################
# Bivariate relationships: zattfam vs predictors
plot(attfam$age, attfam$zattfam,
     pch = ".", col = rgb(0, 0, 0, 0.1),
     xlab = "Age", ylab = "zattfam",
     main = "Attitudes vs age")
lines(lowess(attfam$age, attfam$zattfam), col = "red",  lwd = 2)
abline(lm(zattfam ~ age, data = attfam),  col = "blue", lwd = 1, lty = 2)

boxplot(zattfam ~ female, data = attfam,
        main = "Attitudes by gender",
        ylab = "zattfam", col = c("lightblue", "pink"))

boxplot(zattfam ~ qualhi, data = attfam,
        main = "Attitudes by highest qualification",
        ylab = "zattfam", col = c("lightyellow", "lightgreen"))

boxplot(zattfam ~ partner, data = attfam,
        main = "Attitudes by partner status",
        ylab = "zattfam", col = c("lightblue", "lightyellow"))

boxplot(zattfam ~ country, data = attfam,
        main = "Attitudes by country",
        ylab = "zattfam", col = "lightgreen")

############################################################
# Centre age at first occasion (age 20)
attfam$age20 <- attfam$age - 20

############################################################
# Step 1: Null model
############################################################
sl.m <- lm(zattfam ~ 1, data = attfam)
summary(sl.m)

vc.m <- lmer(zattfam ~ (1 | pid), data = attfam, REML = FALSE)
anova(vc.m, sl.m)
summary(vc.m)

##############################################################
# ICC
var.comp.null <- as.data.frame(VarCorr(vc.m), comp = "Variance")
sig2u0.null   <- var.comp.null$vcov[1]   
sig2e.null    <- var.comp.null$vcov[2]   

# Then ICC will work
icc <- sig2u0.null / (sig2u0.null + sig2e.null)
cat("ICC:", round(icc, 3), "\n")

# Extract variance components
var.comp <- as.data.frame(VarCorr(vc.m), comp = "Variance")
var.comp
sig2u0.null <- var.comp$vcov[1] 
sig2e.null  <- var.comp$vcov[2]   

# 95% plausible values range for individual means 
beta0.null <- fixef(vc.m)[1]
sd.u0.null <- sqrt(sig2u0.null)
cat("95% PVR for individual means:",
    round(beta0.null - 1.96 * sd.u0.null, 3), "to",
    round(beta0.null + 1.96 * sd.u0.null, 3), "\n")


############################################################
# Step 3: Growth curve
############################################################

ri.m <- lmer(zattfam ~ age20 + (1 | pid), data = attfam, REML = FALSE)
summary(ri.m)$coefficients
anova(ri.m, vc.m)

# Extract variance components
sigma.ri <- as.data.frame(VarCorr(ri.m), comp = "Variance")
sigma.ri

# VPC conditional on age
vpc.ri <- sigma.ri$vcov[1] / (sigma.ri$vcov[1] + sigma.ri$vcov[2])
cat("VPC conditional on age:", round(vpc.ri, 3), "\n")


rs.m <- lmer(zattfam ~ age20 + (1 + age20 | pid), data = attfam, REML = FALSE)
summary(rs.m)$coefficients # Convergence warning

############################################################
# Scale the time variable to reduce variance and fix convergence
attfam$age20s <- attfam$age20 / 8

rs.m <- lmer(zattfam ~ age20s + (1 + age20s | pid),
             data = attfam, REML = FALSE)
summary(rs.m)$coefficients

ri.m.s <- lmer(zattfam ~ age20s + (1 | pid),
               data = attfam, REML = FALSE)
anova(rs.m, ri.m.s) # Significant

attfam$age20sq <- attfam$age20s^2
rs.quad.fix <- lmer(zattfam ~ age20s + age20sq + (1 + age20s | pid),
                    data = attfam, REML = FALSE)
anova(rs.quad.fix, rs.m)   # Insignificant

############################################################
# Step 4: Add level 1 covariates
############################################################

m.l1 <- lmer(zattfam ~ age20s + qualhi + partner + country +
               (1 + age20s | pid),
             data = attfam, REML = FALSE)
summary(m.l1)$coefficients
anova(m.l1, rs.m) # Significant

m.l1.no.country <- lmer(zattfam ~ age20s + qualhi + partner +
                          (1 + age20s | pid),
                        data = attfam, REML = FALSE)
anova(m.l1.no.country, m.l1) # Insignificant

m.l1.no.qualhi <- lmer(zattfam ~ age20s + partner + country +
                         (1 + age20s | pid),
                       data = attfam, REML = FALSE)
anova(m.l1.no.qualhi, m.l1) # Borderline

m.l1.no.qualhi.country <- lmer(zattfam ~ age20s + partner +
                                 (1 + age20s | pid),
                               data = attfam, REML = FALSE)
anova(m.l1.no.qualhi.country,m.l1.no.qualhi) # Insignificant
anova(m.l1.no.qualhi.country, m.l1) # Borderline 
anova(m.l1.no.qualhi.country, rs.m) # Significant


m.l1.final <- m.l1.no.qualhi.country
summary(m.l1.final)$coefficients

############################################################
# Step 5: Add level 2 covariate — female
############################################################

m.l2 <- lmer(zattfam ~ age20s + partner + female +
               (1 + age20s | pid),
             data = attfam, REML = FALSE)

anova(m.l2, m.l1.final) # Significant
summary(m.l2)$coefficients

############################################################
# Step 6: Test for autocorrelated residuals 
############################################################

# Refit m.l2 using nlme for comparison
m.l2.nlme <- lme(zattfam ~ age20s + partner + female,
                 random = ~ 1 + age20s | pid,
                 data = attfam,
                 method = "ML")

# Add AR(1) residual structure
m.l2.ar1 <- lme(zattfam ~ age20s + partner + female,
                random = ~ 1 + age20s | pid,
                correlation = corAR1(form = ~ age20 | pid),
                data = attfam,
                method = "ML")


anova(m.l2.nlme, m.l2.ar1) # Insignificant

final.model <- m.l2   

############################################################
# Step 7: Residual diagnostics on final model
############################################################
# Final model fixed effects
summary(final.model)$coefficients

# Intermediate model comparisons — track coefficient changes
summary(rs.m)$coefficients # before adding covariates
summary(m.l1.final)$coefficients # after adding partner
summary(m.l2)$coefficients # after adding female (final model)

##############################################################
# Random part variance components and correlations
sigma.final.q2 <- as.data.frame(VarCorr(final.model), comp = "Variance")
sigma.final.q2

# VPC of final model
sig2u0.final.q2 <- sigma.final.q2$vcov[1]
sig2e.final.q2  <- sigma.final.q2$vcov[4]
vpc.final.q2    <- sig2u0.final.q2 / (sig2u0.final.q2 + sig2e.final.q2)
cat("VPC final model:", round(vpc.final.q2, 3), "\n")

##############################################################
# 95% PVRs for intercepts and slopes 
beta0.final.q2 <- fixef(final.model)[1]
beta1.final.q2 <- fixef(final.model)["age20s"]
sd.u0.final.q2 <- sqrt(sigma.final.q2$vcov[1])
sd.u1.final.q2 <- sqrt(sigma.final.q2$vcov[2])

cat("95% PVR intercepts (attitude at age 20):",
    round(beta0.final.q2 - 1.96 * sd.u0.final.q2, 3), "to",
    round(beta0.final.q2 + 1.96 * sd.u0.final.q2, 3), "\n")

# Convert slope back to per-year units (divide by 8 since age20s = age20/8)
beta1.peryear <- beta1.final.q2 / 8
cat("Average annual change in zattfam (per year):",
    round(beta1.peryear, 4), "\n")

cat("95% PVR slopes (per 8-year unit of age20s):",
    round(beta1.final.q2 - 1.96 * sd.u1.final.q2, 3), "to",
    round(beta1.final.q2 + 1.96 * sd.u1.final.q2, 3), "\n")

# Convert PVR to per-year units
cat("95% PVR slopes (per year):",
    round((beta1.final.q2 - 1.96 * sd.u1.final.q2) / 8, 4), "to",
    round((beta1.final.q2 + 1.96 * sd.u1.final.q2) / 8, 4), "\n")

##############################################################
# Between-individual variance as function of age 
sig2u0.fq2  <- sigma.final.q2$vcov[1]
sig2u1.fq2  <- sigma.final.q2$vcov[2]
sigu01.fq2  <- sigma.final.q2$vcov[3]

age.range.s <- seq(0, 2, length = 100)   # age20s ranges 0 to 16/8 = 2
btwn.var.q2 <- sig2u0.fq2 + 2 * sigu01.fq2 * age.range.s +
  sig2u1.fq2 * age.range.s^2

plot(age.range.s * 8 + 20, btwn.var.q2,
     type = "l", lwd = 2, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Between-individual variance",
     main = "Between-individual variance as function of age — final model")
abline(h = sig2u0.fq2, lty = 2, col = "grey50")

##############################################################
# Cumulative R2 from null to final model
R2_total_q2 <- ((sig2u0.null + sig2e.null) -
                  (sig2u0.final.q2 + sig2e.final.q2)) /
  (sig2u0.null + sig2e.null)
R2_1_q2     <- (sig2e.null - sig2e.final.q2) / sig2e.null
R2_2_q2     <- (sig2u0.null - sig2u0.final.q2) / sig2u0.null

cat("Cumulative R2 total (null to final):", round(R2_total_q2, 3), "\n")
cat("Cumulative R2 L1:                   ", round(R2_1_q2,     3), "\n")
cat("Cumulative R2 L2:                   ", round(R2_2_q2,     3), "\n")

##############################################################
# Residual diagnostics on final model 
# Level 1 residuals
hist(residuals(final.model),
     xlab = "Residual",
     main = "Histogram of level-1 residuals — final model")

qqnorm(residuals(final.model),
       main = "QQ plot of level-1 residuals — final model")
qqline(residuals(final.model), col = "red")

plot(fitted(final.model), residuals(final.model, scaled = TRUE),
     pch = ".", col = rgb(0, 0, 0, 0.3),
     xlab = "Fitted values",
     ylab = "Standardised residual",
     main = "Residuals vs fitted — final model")
abline(h = 0, col = "red")

##############################################################
# Level 2 residuals
uest.final.q2 <- ranef(final.model)
u.final.q2    <- uest.final.q2[[1]]
use.final.q2  <- sqrt(attr(uest.final.q2[[1]], "postVar")[1, 1, ])

# Confirm no NaN issues
cat("Number of NaNs:", sum(is.nan(use.final.q2)), "\n")

# Standardised individual intercept residuals
u.final.st.q2 <- u.final.q2[, 1] / use.final.q2

hist(u.final.st.q2,
     xlab = "Standardised individual residual",
     main = "Histogram of individual residuals — final model")

qqnorm(u.final.st.q2,
       main = "QQ plot of individual residuals — final model")
qqline(u.final.st.q2, col = "red")

##############################################################
# Caterpillar plot of individual effects from final model
u.df.q2      <- data.frame(u0 = u.final.q2[, 1], se = use.final.q2)
u.df.q2      <- u.df.q2[order(u.df.q2$u0), ]
u.df.q2$rank <- 1:nrow(u.df.q2)

plot(u.df.q2$rank, u.df.q2$u0, type = "n",
     xlab = "Rank of individual effect",
     ylab = "Estimate of individual effect (u0)",
     main = "Caterpillar plot of individual effects — final model")
segments(u.df.q2$rank,
         u.df.q2$u0 - 1.96 * u.df.q2$se,
         u.df.q2$rank,
         u.df.q2$u0 + 1.96 * u.df.q2$se,
         col = "black")
points(u.df.q2$rank, u.df.q2$u0,
       col = "blue", pch = 16, cex = 0.5)
abline(h = 0, col = "red", lty = 2)