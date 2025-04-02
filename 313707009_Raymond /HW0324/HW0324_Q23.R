if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("cocaine")

model <- lm(price ~ quant + qual + trend, data = cocaine)
# (b) 
summary(model)
# (c) R-squared
summary_model <- summary(model)
r_squared <- summary_model$r.squared
r_squared
# (d) beta2(QUANT)之檢定
quant_coef <- summary_model$coefficients["quant", "Estimate"]
quant_p_value <- summary_model$coefficients["quant", "Pr(>|t|)"]

if (quant_p_value / 2 < 0.05 && quant_coef < 0) {
  cat("拒絕 H0: 存在負相關關係，即銷售量越大，價格越低。\n")
} else {
  cat("未能拒絕 H0: 沒有足夠的證據支持銷售量與價格之間存在負相關關係。\n")
}
#(e) beta3(QUAL)之檢定
qual_coef <- summary_model$coefficients["qual", "Estimate"]
qual_p_value <- summary_model$coefficients["qual", "Pr(>|t|)"]

if (qual_p_value < 0.05 && qual_coef > 0) {
  cat("拒絕 H0: 可卡因品質對價格有顯著影響，且品質越好價格越高。\n")
} else {
  cat("未能拒絕 H0: 沒有足夠證據表明品質對價格有顯著影響。\n")
}
#(f) 價格變化平均
trend_coef <- summary(model)$coefficients["trend", "Estimate"]

trend_coef