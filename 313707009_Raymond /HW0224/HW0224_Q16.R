# 安裝並載入必要的套件
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

# 載入 CAPM 數據
library(POE5Rdata)
data("capm5")
capm_data <- capm5

firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")
capm_data_adj <- capm_data  
capm_data_adj$mkt <- capm_data$mkt - capm_data$riskfree 
# 調整每間公司的超額收益率
for (firm in firms) {
  capm_data_adj[[firm]] <- capm_data[[firm]] - capm_data$riskfree
}
models <- list()
betas <- c()
# (b)小題 CAPM模型與beta值
for (firm in firms) {
  formula <- as.formula(paste(firm, " ~ mkt", sep = ""))
  models[[firm]] <- lm(formula, data = capm_data_adj)
  betas[firm] <- coef(models[[firm]])["mkt"]
}

print(betas)

# 找出最大與最小的 beta 值
most_aggressive <- names(which.max(betas))  # Beta 最大（最激進）
most_defensive <- names(which.min(betas))  # Beta 最小（最保守）

cat("最激進股票:", most_aggressive, "Beta =", max(betas), "\n")
cat("最保守股票:", most_defensive, "Beta =", min(betas), "\n")

# (c)小題 異常報酬
alphas <- sapply(models, function(model) coef(model)[1])
alphas

alpha_pvalues <- sapply(models, function(model) summary(model)$coefficients[1,4])
alpha_pvalues
significant_alpha <- names(alpha_pvalues[alpha_pvalues < 0.05])

cat("有異常報酬 的股票:", significant_alpha, "\n")
msft_model <- models[["msft"]]
alpha_msft <- coef(msft_model)[1]  # 截距 (Alpha)
beta_msft <- coef(msft_model)[2]   # 斜率 (Beta)

# 繪製調整後的 MSFT CAPM 回歸分析圖
plot(capm_data_adj$mkt, capm_data_adj$msft, 
     main = "MSFT 的 CAPM 回歸分析 (超額收益)",
     xlab = "市場超額收益率 (MKT - riskfree)", 
     ylab = "MSFT 超額收益率 (MSFT - riskfree)", 
     col = "blue", 
     pch = 16)  # pch 設定點的形狀

# 加入回歸線
abline(a = alpha_msft, b = beta_msft, col = "red", lwd = 2)
# (d)小題 alpha設定0
models_0 <- list()
betas_0 <- c()

# 重新執行 CAPM 回歸，去掉截距 (Alpha = 0)
for (firm in firms) {
  formula_0 <- as.formula(paste(firm, " ~ 0 + mkt", sep = ""))  # **0 + mkt 表示無截距**
  models_0[[firm]] <- lm(formula_0, data = capm_data_adj)
  betas_0[firm] <- coef(models_0[[firm]])["mkt"]
}

print(betas_0)