#2.16 (b)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("capm5")

firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")

for (firm in firms) {
  formula <- as.formula(paste(firm, "~ mkt + riskfree"))
  model <- lm(formula, data = capm5)
  cat("\nFirm:", firm, "\n")
  print(summary(model))
}

#2.16 (c)

library(POE5Rdata)
data("capm5")

sum(is.na(capm5$mkt))  # 檢查 MKT 是否有 NA 值
sum(is.na(capm5$msft))  # 檢查 Microsoft 是否有 NA 值
sum(!is.finite(capm5$mkt))  # 檢查 MKT 是否有 Inf 值
sum(!is.finite(capm5$msft))  # 檢查 Microsoft 是否有 Inf 值

capm5_clean <- capm5[!is.na(capm5$mkt) & !is.na(capm5$msft) & is.finite(capm5$mkt) & is.finite(capm5$msft), ]

model_msft <- lm(msft ~ mkt + riskfree, data = capm5_clean)

summary(model_msft)

plot(capm5_clean$mkt, capm5_clean$msft, 
     main = "Microsoft 迴歸結果", 
     xlab = "Market Return (MKT)", 
     ylab = "Microsoft Return",
     pch = 19, col = "blue")

abline(model_msft, col = "red", lwd = 2) 

#2.16 (d)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("capm5")

firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")


for (firm in firms) {
  formula_no_intercept <- as.formula(paste(firm, "~ mkt + riskfree - 1")) 
  model_no_intercept <- lm(formula_no_intercept, data = capm5)
  cat("\nFirm:", firm, "without intercept\n")
  print(summary(model_no_intercept))
