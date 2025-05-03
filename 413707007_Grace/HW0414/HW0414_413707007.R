
#8.16
load("C:/Users/thaol/Downloads/vacation.rdata")
print(vacation)
model <- lm(miles ~ income + age + kids, data=vacation) 
summary(model)
confint(model, level = 0.95)  
residuals <- resid(model)

plot(vacation$income, residuals,
     main = "OLS Residuals vs INCOME",
     xlab = "INCOME ($1000)", ylab = "Residuals",
     pch = 19, col = "blue")

plot(vacation$age, residuals,
     main = "OLS Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals",
     pch = 19, col = "darkgreen")

#    — split into the lowest and highest 90 observations by income
vacation_sorted <- vacation[order(vacation$income), ]
n <- nrow(vacation_sorted)
data_low  <- vacation_sorted[1:90, ]          # first 90 rows (lowest incomes)
data_high <- vacation_sorted[(n-89):n, ]      # last 90 rows (highest incomes)
model_low  <- lm(miles ~ income + age + kids, data = data_low)
model_high <- lm(miles ~ income + age + kids, data = data_high)
summary(model_low)
summary(model_high)
SSE_low  <- sum(resid(model_low )^2)
SSE_high <- sum(resid(model_high)^2)
alpha <- 0.05
df    <- 90 - 4                              # degrees of freedom: sample size minus number of parameters
F_stat           <- (SSE_high/df) / (SSE_low/df)
F_critical_upper <- qf(1 - alpha, df, df)    # upper 5% point of the F‐distribution
print(F_stat)
print(F_critical_upper)


install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)
# 3. Compute the robust (HC1) covariance matrix and SE’s
robust_vcov  <- vcovHC(model, type = "HC1")
robust_se    <- sqrt(diag(robust_vcov))
# 4. Extract the point estimate and SE for the ‘kids’ coefficient
beta_kids       <- coef(model)["kids"]
se_classic_kids <- summary(model)$coefficients["kids","Std. Error"]
se_robust_kids  <-    robust_se["kids"]

# 5. Get the t‐critical value for 95% CI
df     <- df.residual(model)
tcrit  <- qt(0.975, df)

# 6. Build both intervals
ci_classic <- beta_kids + c(-1,1) * tcrit * se_classic_kids
ci_robust  <- beta_kids + c(-1,1) * tcrit * se_robust_kids
print(ci_classic)
print(ci_robust)

library(broom)
library(knitr)
# to multiply the model by 1/sqrt(xi), weight should be wi=1/xi.
# With known form of variances
w <- 1/vacation$income^2
model <- lm(miles ~ income + age + kids, data=vacation)  #OLS
model.wls <- lm(miles ~ income + age + kids, weights=w, data=vacation) #GLS
vcvmodel <- coeftest(model, vcov.=robust_vcov) # White SE
gls_robust_se <- vcovHC(model.wls, type = "HC1")
vcvmodel_gls <- coeftest(model.wls, vcov.=gls_robust_se) # White SE
kable(tidy(model),caption="OLS estimates for the miles equation")

kable(tidy(vcvmodel),caption="OLS estimates for the miles equation with robust se")
      
kable(tidy(model.wls),caption="WLS estimates for the miles equation")

kable(tidy(vcvmodel_gls),caption="WLS estimates for the miles equation with robust se")

confint(model.wls, level = 0.95)  
coefci(model.wls, vcov. = gls_robust_se, level = 0.95)    # GLS （ robust SE）

#8.18
load("C:/Users/thaol/Downloads/cps5.rdata")
print(cps5)
install.packages("dplyr")

# Then load it:
library(dplyr)


cps5_male <- cps5 %>% filter(female == 0)
cps5_female <- cps5 %>% filter(female == 1)
model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = cps5_male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = cps5_female)
rss_male <- sum(resid(model_male)^2)
rss_female <- sum(resid(model_female)^2)
df_male <- model_male$df.residual
df_female <- model_female$df.residual
F_stat <- (rss_female / df_female) / (rss_male / df_male)
qf(0.05/2, df_male, df_female)
qf(1-0.05/2, df_male, df_female)
print(F_stat)



model818 <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
residuals_squared <- resid(model818)^2
Mod_case1 <- lm(residuals_squared ~ metro + female + black, data = cps5)
n <- nrow(cps5)
R_squared_case1 <- summary(Mod_case1)$r.squared
NR2 <- n * R_squared_case1
df <- length(coef(Mod_case1)) - 1
critical_case1 <- qchisq(0.99, df)
print(NR2)
print(critical_case1)

Mod_case2 <- lm(residuals_squared ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west , data = cps5)
R_squared_case2 <- summary(Mod_case2)$r.squared
NR2_2 <- n * R_squared_case2
df <- length(coef(Mod_case2)) - 1
critical_case2 <- qchisq(0.99, df)
print(NR2_2)
print(critical_case2)


white_test <- bptest(model818, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west + I(educ^2) + I(exper^2) + I(female^2) + I(black^2) + I(metro^2) + I(south^2) + I(midwest^2) + I(west^2), data = cps5)
print(white_test)


summary(model818)
# d.
# Load necessary packages
library(sandwich)
library(lmtest)

# Estimate model with conventional standard errors
conventional_se <- summary(model818)

# Estimate model with robust standard errors
robust_se818 <- coeftest(model818, vcov = vcovHC(model818, type = "HC1"))

# Function to calculate confidence intervals
calculate_ci <- function(coef, se, df, level = 0.95) {
  alpha <- 1 - level
  t_crit <- qt(1 - alpha/2, df)
  lower <- coef - t_crit * se
  upper <- coef + t_crit * se
  return(c(lower, upper))
}

# Compare confidence intervals
df_residual <- model818$df.residual
variables <- rownames(robust_se818)

# Create data frame for comparison
ci_comparison <- data.frame(
  Variable = variables,
  Conventional_SE = NA,
  Robust_SE = NA,
  Conv_CI_Lower = NA,
  Conv_CI_Upper = NA,
  Robust_CI_Lower = NA,
  Robust_CI_Upper = NA,
  Width_Change = NA
)

for (i in 1:length(variables)) {
  var <- variables[i]
  conv_se <- conventional_se$coefficients[var, "Std. Error"]
  rob_se <- robust_se818[var, "Std. Error"]
  
  conv_ci <- calculate_ci(coef(model818)[var], conv_se, df_residual)
  rob_ci <- calculate_ci(coef(model818)[var], rob_se, df_residual)
  
  conv_width <- conv_ci[2] - conv_ci[1]
  rob_width <- rob_ci[2] - rob_ci[1]
  width_change <- (rob_width - conv_width) / conv_width * 100
  
  ci_comparison[i, "Conventional_SE"] <- conv_se
  ci_comparison[i, "Robust_SE"] <- rob_se
  ci_comparison[i, "Conv_CI_Lower"] <- conv_ci[1]
  ci_comparison[i, "Conv_CI_Upper"] <- conv_ci[2]
  ci_comparison[i, "Robust_CI_Lower"] <- rob_ci[1]
  ci_comparison[i, "Robust_CI_Upper"] <- rob_ci[2]
  ci_comparison[i, "Width_Change"] <- width_change
}

# Display results
print(ci_comparison)

# Summarize which coefficients have wider/narrower intervals
wider <- ci_comparison$Variable[ci_comparison$Width_Change > 0]
narrower <- ci_comparison$Variable[ci_comparison$Width_Change < 0]

cat("\nCoefficients with wider intervals using robust SE:\n")
print(wider)

cat("\nCoefficients with narrower intervals using robust SE:\n")
print(narrower)

# e.
# Step 1: Estimate the model by OLS
model_ols <- model818

# Step 2: Calculate squared residuals
ehatsq <- residuals(model_ols)^2

# Step 3: Regress log of squared residuals on METRO and EXPER
sighatsq.ols <- lm(log(ehatsq) ~ metro + exper, data = cps5)

# Step 4: Generate predicted variances
vari <- exp(fitted(sighatsq.ols))

# Step 5: Estimate the model using FGLS with these weights
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5, weights = 1/vari)

# Summary of FGLS model
summary_fgls <- summary(model_fgls)

# Compare confidence intervals
fgls_coefs <- coef(model_fgls)
fgls_se <- summary_fgls$coefficients[, "Std. Error"]

# Create data frame for comparison with OLS robust
ci_comparison_fgls <- data.frame(
  Variable = variables,
  FGLS_SE = NA,
  Robust_SE = NA,
  FGLS_CI_Lower = NA,
  FGLS_CI_Upper = NA,
  Robust_CI_Lower = ci_comparison$Robust_CI_Lower,
  Robust_CI_Upper = ci_comparison$Robust_CI_Upper,
  Width_Change = NA
)

for (i in 1:length(variables)) {
  var <- variables[i]
  fgls_se_val <- summary_fgls$coefficients[var, "Std. Error"]
  
  fgls_ci <- calculate_ci(fgls_coefs[var], fgls_se_val, model_fgls$df.residual)
  
  fgls_width <- fgls_ci[2] - fgls_ci[1]
  rob_width <- ci_comparison$Robust_CI_Upper[i] - ci_comparison$Robust_CI_Lower[i]
  width_change <- (fgls_width - rob_width) / rob_width * 100
  
  ci_comparison_fgls[i, "FGLS_SE"] <- fgls_se_val
  ci_comparison_fgls[i, "Robust_SE"] <- ci_comparison$Robust_SE[i]
  ci_comparison_fgls[i, "FGLS_CI_Lower"] <- fgls_ci[1]
  ci_comparison_fgls[i, "FGLS_CI_Upper"] <- fgls_ci[2]
  ci_comparison_fgls[i, "Width_Change"] <- width_change
}

# Display results
print(ci_comparison_fgls)

# Summarize which coefficients have wider/narrower intervals
fgls_wider <- ci_comparison_fgls$Variable[ci_comparison_fgls$Width_Change > 0]
fgls_narrower <- ci_comparison_fgls$Variable[ci_comparison_fgls$Width_Change < 0]

cat("\nCoefficients with wider intervals using FGLS vs. OLS robust:\n")
print(fgls_wider)

cat("\nCoefficients with narrower intervals using FGLS vs. OLS robust:\n")
print(fgls_narrower)

# f.
# Calculate robust standard errors for FGLS
robust_se_fgls <- coeftest(model_fgls, vcov = vcovHC(model_fgls, type = "HC1"))

# Compare confidence intervals
ci_comparison_robust_fgls <- data.frame(
  Variable = variables,
  FGLS_SE = ci_comparison_fgls$FGLS_SE,
  FGLS_Robust_SE = NA,
  OLS_Robust_SE = ci_comparison$Robust_SE,
  FGLS_CI_Lower = ci_comparison_fgls$FGLS_CI_Lower,
  FGLS_CI_Upper = ci_comparison_fgls$FGLS_CI_Upper,
  FGLS_Robust_CI_Lower = NA,
  FGLS_Robust_CI_Upper = NA,
  OLS_Robust_CI_Lower = ci_comparison$Robust_CI_Lower,
  OLS_Robust_CI_Upper = ci_comparison$Robust_CI_Upper
)

for (i in 1:length(variables)) {
  var <- variables[i]
  fgls_robust_se <- robust_se_fgls[var, "Std. Error"]
  
  fgls_robust_ci <- calculate_ci(fgls_coefs[var], fgls_robust_se, model_fgls$df.residual)
  
  ci_comparison_robust_fgls[i, "FGLS_Robust_SE"] <- fgls_robust_se
  ci_comparison_robust_fgls[i, "FGLS_Robust_CI_Lower"] <- fgls_robust_ci[1]
  ci_comparison_robust_fgls[i, "FGLS_Robust_CI_Upper"] <- fgls_robust_ci[2]
}

# Display results
print(ci_comparison_robust_fgls)

library(dplyr)

# 1) Compute each interval’s width
ci_compare <- ci_comparison_robust_fgls %>%
  mutate(
    Width_FGLS            = FGLS_CI_Upper            - FGLS_CI_Lower,
    Width_FGLS_Robust     = FGLS_Robust_CI_Upper     - FGLS_Robust_CI_Lower,
    Width_OLS_Robust      = OLS_Robust_CI_Upper      - OLS_Robust_CI_Lower,
    # 2) Differences
    Diff_RobustFGLS_vs_FGLS      = Width_FGLS_Robust - Width_FGLS,
    Diff_RobustFGLS_vs_OLSRobust = Width_FGLS_Robust - Width_OLS_Robust
  )

# 3) Which coefficients got wider/narrower?
wider_rf_vs_f   <- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_FGLS >  0]
narrower_rf_vs_f<- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_FGLS <  0]

wider_rf_vs_o   <- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_OLSRobust >  0]
narrower_rf_vs_o<- ci_compare$Variable[ci_compare$Diff_RobustFGLS_vs_OLSRobust <  0]

# 4) Print summaries
cat("\n--- Robust FGLS vs. FGLS ---\n")
cat("Wider under Robust FGLS:\n");    print(wider_rf_vs_f)
cat("Narrower under Robust FGLS:\n"); print(narrower_rf_vs_f)

cat("\n--- Robust FGLS vs. Robust OLS ---\n")
cat("Wider under Robust FGLS:\n");    print(wider_rf_vs_o)
cat("Narrower under Robust FGLS:\n"); print(narrower_rf_vs_o)

# g.
# Summarize all methods for all coefficients in wide format
coefficients <- names(coef(model818))  # Get all coefficients from the model

# Create an empty data frame to store results
summary_wide <- data.frame(
  Method = c("OLS Conventional", "OLS Robust", "FGLS Conventional", "FGLS Robust")
)

# Loop through all coefficients
for (coef_name in coefficients) {
  # Extract coefficients and standard errors
  ols_conv_coef <- coef(model818)[coef_name]
  ols_conv_se <- conventional_se$coefficients[coef_name, "Std. Error"]
  
  ols_rob_coef <- coef(model818)[coef_name]
  ols_rob_se <- robust_se818[coef_name, "Std. Error"]
  
  fgls_conv_coef <- coef(model_fgls)[coef_name]
  fgls_conv_se <- summary_fgls$coefficients[coef_name, "Std. Error"]
  
  fgls_rob_coef <- coef(model_fgls)[coef_name]
  fgls_rob_se <- robust_se_fgls[coef_name, "Std. Error"]
  
  # Add columns to the summary table
  summary_wide[[paste0(coef_name, "_Coef")]] <- c(ols_conv_coef, ols_rob_coef, fgls_conv_coef, fgls_rob_coef)
  summary_wide[[paste0(coef_name, "_SE")]] <- c(ols_conv_se, ols_rob_se, fgls_conv_se, fgls_rob_se)
}

# Print the summary table
print(summary_wide)