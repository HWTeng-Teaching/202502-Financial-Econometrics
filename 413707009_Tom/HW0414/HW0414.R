# Load necessary libraries
library(broom)
library(knitr)
library(car)
library(lmtest)

# 8.6
# Part (a) - Testing variance equality between males and females
n_males <- 577; sse_males <- 97161.9174
n_females <- 423; sigma_f <- 12.024
k <- 4

sigma2_males <- sse_males / (n_males - k)
sigma2_females <- sigma_f^2
f_stat <- sigma2_males / sigma2_females

alpha <- 0.05
f_crit_upper <- qf(1 - alpha/2, n_males - k, n_females - k)
f_crit_lower <- qf(alpha/2, n_males - k, n_females - k)
p_value <- 2 * min(pf(f_stat, n_males - k, n_females - k), 1 - pf(f_stat, n_males - k, n_females - k))

cat("Males' variance:", sigma2_males, "\nFemales' variance:", sigma2_females, 
    "\nF-statistic:", f_stat, "\nCritical F-values:", f_crit_lower, "-", f_crit_upper,
    "\nP-value:", p_value, "\n")
cat(ifelse(f_stat > f_crit_upper || f_stat < f_crit_lower, 
           "Reject H0: Variances differ.\n", "Fail to reject H0: Variances are equal.\n"))

# Part (b) - Comparing variances between single and married individuals
n_single <- 400; sse_single <- 56231.0382
n_married <- 600; sse_married <- 100703.0471
k <- 5

sigma2_single <- sse_single / (n_single - k)
sigma2_married <- sse_married / (n_married - k)
f_stat <- sigma2_married / sigma2_single
f_crit <- qf(1 - alpha, n_married - k, n_single - k)
p_value <- 1 - pf(f_stat, n_married - k, n_single - k)

cat("\nSingle variance:", sigma2_single, "\nMarried variance:", sigma2_married,
    "\nF-statistic:", f_stat, "\nCritical F-value:", f_crit, "\nP-value:", p_value, "\n")
cat(ifelse(f_stat > f_crit, 
           "Reject H0: Married individuals have greater variance.\n", 
           "Fail to reject H0.\n"))

# Part (c) - NR^2 Test for Heteroskedasticity
NR2 <- 59.03; df <- 4
chi_sq_crit <- qchisq(1 - alpha, df)
p_value <- 1 - pchisq(NR2, df)

cat("\nNR^2:", NR2, "\nChi-sq critical:", chi_sq_crit, "\nP-value:", p_value, "\n")
cat(ifelse(NR2 > chi_sq_crit, 
           "Reject H0: Evidence of heteroskedasticity.\n", 
           "Fail to reject H0.\n"))

# Part (d) - Chi-squared critical value and p-value
critical_value <- qchisq(0.95, 14)
p_value <- 1 - pchisq(78.82, 14)

cat("\nChi-sq critical (df=14):", critical_value, "\nP-value for test statistic 78.82:", p_value, "\n")

# Part (e) and (f) - Refer to the answers provided in PDF.

# ==============================================================================
# 8.16 - Vacation Data Example
# Load dataset
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
load(url(url, "rb"))

# Rename variables to uppercase
names(vacation) <- toupper(names(vacation))

# Part (a) - OLS estimation
model_ols <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation)
summary(model_ols)
confint(model_ols, "KIDS", level = 0.95)

# Part (b) - Residual plots
par(mfrow = c(1, 2))
plot(vacation$INCOME, residuals(model_ols), main = "Residuals vs INCOME", xlab = "Income ($1000)", ylab = "Residuals"); abline(h = 0, lty = 2)
plot(vacation$AGE, residuals(model_ols), main = "Residuals vs AGE", xlab = "Age", ylab = "Residuals"); abline(h = 0, lty = 2)
par(mfrow = c(1,1))

# Part (c) - Goldfeld-Quandt Test
vacation_sorted <- vacation[order(vacation$INCOME), ]
group1 <- vacation_sorted[1:90, ]
group2 <- vacation_sorted[111:200, ]

model_group1 <- lm(MILES ~ INCOME + AGE + KIDS, data = group1)
model_group2 <- lm(MILES ~ INCOME + AGE + KIDS, data = group2)

rss1 <- sum(residuals(model_group1)^2)
rss2 <- sum(residuals(model_group2)^2)

df1 <- nrow(group1) - 4
df2 <- nrow(group2) - 4

f_statistic <- (rss2 / df2) / (rss1 / df1)
f_critical <- qf(0.95, df2, df1)
p_value <- 1 - pf(f_statistic, df2, df1)

cat("\nGQ F-statistic:", f_statistic, "\nCritical value:", f_critical, "\nP-value:", p_value, "\n")
cat(ifelse(p_value < 0.05, 
           "Reject H0: Evidence of heteroskedasticity.\n", 
           "Fail to reject H0.\n"))

# Part (d) - Robust vs Regular SE
kable(tidy(model_ols), caption = "Regular Standard Errors")
robust_se <- coeftest(model_ols, vcov. = hccm(model_ols, type = "hc1"))
kable(tidy(robust_se), caption = "Robust Standard Errors")

# Calculate robust CI manually
kids_coef <- coef(model_ols)["KIDS"]
kids_se <- robust_se["KIDS", "Std. Error"]
t_crit <- qt(0.975, model_ols$df.residual)
robust_ci <- kids_coef + c(-1, 1) * t_crit * kids_se

cat("\nRobust 95% CI for KIDS:", robust_ci[1], "to", robust_ci[2], "\n")

# Part (e) - GLS Estimation
weights <- 1 / vacation$INCOME^2
model_gls <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation, weights = weights)
summary(model_gls)

# GLS CIs
kids_gls_coef <- coef(model_gls)["KIDS"]
kids_gls_se <- summary(model_gls)$coefficients["KIDS", "Std. Error"]
kids_gls_ci <- kids_gls_coef + c(-1, 1) * t_crit * kids_gls_se

cat("\nGLS 95% CI for KIDS:", kids_gls_ci[1], "to", kids_gls_ci[2], "\n")

# -----------------------------------------------------------------------------
# 8.18 - CPS5 Data Example
# Load dataset
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata"
load(url(url, "rb"))

names(cps5) <- toupper(names(cps5))
cps5$EXPER2 <- cps5$EXPER^2

# Part (a) - Gender specific models
males <- subset(cps5, FEMALE == 0)
females <- subset(cps5, FEMALE == 1)

model_males <- lm(log(WAGE) ~ EDUC + EXPER + EXPER2 + BLACK + METRO + SOUTH + MIDWEST + WEST, data = males)
model_females <- lm(log(WAGE) ~ EDUC + EXPER + EXPER2 + BLACK + METRO + SOUTH + MIDWEST + WEST, data = females)

rss_males <- sum(residuals(model_males)^2)
rss_females <- sum(residuals(model_females)^2)

df_males <- nrow(males) - length(coef(model_males))
df_females <- nrow(females) - length(coef(model_females))

var_males <- rss_males / df_males
var_females <- rss_females / df_females

if (var_males > var_females) {
  f_stat <- var_males / var_females
  df1 <- df_males; df2 <- df_females
} else {
  f_stat <- var_females / var_males
  df1 <- df_females; df2 <- df_males
}

f_crit_upper <- qf(0.975, df1, df2)
f_crit_lower <- qf(0.025, df1, df2)
p_value <- 2 * min(pf(f_stat, df1, df2), 1 - pf(f_stat, df1, df2))

cat("\nVariance Males:", var_males, "Variance Females:", var_females,
    "\nF-statistic:", f_stat, "Critical values:", f_crit_lower, "-", f_crit_upper, 
    "\nP-value:", p_value, "\n")
cat(ifelse(f_stat > f_crit_upper || f_stat < f_crit_lower, 
           "Reject H0: Variances differ.\n", 
           "Fail to reject H0.\n"))

# (b), (c), (d) follow similarly - Breusch-Pagan test, NR2 manual, White Test, Robust SE

# -----------------------------------------------------------------------------

#TITLE: TODO
