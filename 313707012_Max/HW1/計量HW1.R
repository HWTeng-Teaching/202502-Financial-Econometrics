library(POE5Rdata)
data(capm5)
?capm5
# b小題
# 計算超額回報
capm5$GE_excess <- capm5$ge - capm5$riskfree
capm5$IBM_excess <- capm5$ibm - capm5$riskfree
capm5$Ford_excess <- capm5$ford - capm5$riskfree
capm5$Microsoft_excess <- capm5$msft - capm5$riskfree
capm5$Disney_excess <- capm5$dis - capm5$riskfree
capm5$ExxonMobil_excess <- capm5$xom - capm5$riskfree
capm5$MKT_excess <- capm5$mkt - capm5$riskfree

# 為每個公司運行 CAPM 回歸
model_GE <- lm(GE_excess ~ MKT_excess, data = capm5)
beta_GE <- coef(model_GE)[2]

model_IBM <- lm(IBM_excess ~ MKT_excess, data = capm5)
beta_IBM <- coef(model_IBM)[2]

model_Ford <- lm(Ford_excess ~ MKT_excess, data = capm5)
beta_Ford <- coef(model_Ford)[2]

model_Microsoft <- lm(Microsoft_excess ~ MKT_excess, data = capm5)
beta_Microsoft <- coef(model_Microsoft)[2]

model_Disney <- lm(Disney_excess ~ MKT_excess, data = capm5)
beta_Disney <- coef(model_Disney)[2]

model_ExxonMobil <- lm(ExxonMobil_excess ~ MKT_excess, data = capm5)
beta_ExxonMobil <- coef(model_ExxonMobil)[2]

# 收集 beta 值
betas <- c(GE = beta_GE, IBM = beta_IBM, Ford = beta_Ford, 
           Microsoft = beta_Microsoft, Disney = beta_Disney, 
           ExxonMobil = beta_ExxonMobil)

# 打印 beta 值
cat("Estimated Beta Values:\n")
print(betas)

# 找出最激進和最保守的公司
max_beta_idx <- which.max(betas)
min_beta_idx <- which.min(betas)

most_aggressive <- names(betas)[which.max(betas)]
most_defensive <- names(betas)[which.min(betas)]

cat("\nMost Aggressive Firm (highest beta):", most_aggressive, 
    "with beta =", betas[most_aggressive], "\n")
cat("Most Defensive Firm (lowest beta):", most_defensive, 
    "with beta =", betas[most_defensive], "\n")

# c小題
# Print the summary to check the intercept (alpha)
summary(model_Microsoft)

# Extract the intercept (alpha) and its p-value
alpha_Microsoft <- coef(model_Microsoft)[1]
p_value_alpha <- summary(model_Microsoft)$coefficients[1, 4]

# Comment on whether alpha is statistically zero
if (p_value_alpha < 0.05) {
  cat("The intercept (alpha) for Microsoft is", round(alpha_Microsoft, 4),
      "with a p-value of", round(p_value_alpha, 4),
      ", indicating it is statistically different from zero at the 5% significance level.\n")
} else {
  cat("The intercept (alpha) for Microsoft is", round(alpha_Microsoft, 4),
      "with a p-value of", round(p_value_alpha, 4),
      ", indicating it is not statistically different from zero at the 5% significance level.\n")
}

# Plot the scatter plot with the fitted regression line
plot(capm5$MKT_excess, capm5$Microsoft_excess,
     xlab = "Market Excess Return (MKT - RISKFREE)",
     ylab = "Microsoft Excess Return (Microsoft - RISKFREE)",
     main = "CAPM Regression for Microsoft",
     pch = 16, col = "blue")
abline(model_Microsoft, col = "red", lwd = 2)  # Add regression line

# d小題
# Use -1 in the formula to remove the intercept
model_GE_no_intercept <- lm(GE_excess ~ MKT_excess - 1, data = capm5)
beta_GE_no_intercept <- coef(model_GE_no_intercept)[1]

model_IBM_no_intercept <- lm(IBM_excess ~ MKT_excess - 1, data = capm5)
beta_IBM_no_intercept <- coef(model_IBM_no_intercept)[1]

model_Ford_no_intercept <- lm(Ford_excess ~ MKT_excess - 1, data = capm5)
beta_Ford_no_intercept <- coef(model_Ford_no_intercept)[1]

model_Microsoft_no_intercept <- lm(Microsoft_excess ~ MKT_excess - 1, data = capm5)
beta_Microsoft_no_intercept <- coef(model_Microsoft_no_intercept)[1]

model_Disney_no_intercept <- lm(Disney_excess ~ MKT_excess - 1, data = capm5)
beta_Disney_no_intercept <- coef(model_Disney_no_intercept)[1]

model_ExxonMobil_no_intercept <- lm(ExxonMobil_excess ~ MKT_excess - 1, data = capm5)
beta_ExxonMobil_no_intercept <- coef(model_ExxonMobil_no_intercept)[1]

# Collect betas without intercept
betas_no_intercept <- c(GE = beta_GE_no_intercept, IBM = beta_IBM_no_intercept, 
                        Ford = beta_Ford_no_intercept, Microsoft = beta_Microsoft_no_intercept, 
                        Disney = beta_Disney_no_intercept, ExxonMobil = beta_ExxonMobil_no_intercept)

# Compare the betas
cat("Betas with Intercept:\n")
print(betas)
cat("\nBetas without Intercept (alpha = 0):\n")
print(betas_no_intercept)

# Calculate the percentage change in betas
percent_change <- ((betas_no_intercept - betas) / betas) * 100
cat("\nPercentage Change in Betas (without intercept vs with intercept):\n")
print(percent_change)

# Comment on whether the betas change much
cat("\nComment on Beta Changes:\n")
for (firm in names(percent_change)) {
  change <- percent_change[firm]
  if (abs(change) < 5) {
    cat(firm, ": The beta changes by ", round(change, 2), 
        "%, which is a small change (less than 5%).\n", sep = "")
  } else {
    cat(firm, ": The beta changes by ", round(change, 2), 
        "%, which is a noticeable change (more than 5%).\n", sep = "")
  }
}
