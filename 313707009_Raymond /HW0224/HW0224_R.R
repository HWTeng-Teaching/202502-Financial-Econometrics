# 載入數據
capm_data = data("capm5")

firms <- c("GE", "IBM", "Ford", "Microsoft", "Disney", "ExxonMobil")
models <- list()
betas <- c()
# (b)小題 CAPM模型與beta值
for (firm in firms) {
  formula <- as.formula(paste(firm, " ~ MKT", sep = ""))
  models[[firm]] <- lm(formula, data = capm_data)
  betas[firm] <- coef(models[[firm]])["MKT"]
}

betas

# 找出最大與最小的 beta 值
most_aggressive <- names(which.max(betas))  # Beta 最大（最激進）
most_defensive <- names(which.min(betas))  # Beta 最小（最保守）

cat("最激進股票:", most_aggressive, "Beta =", max(betas), "\n")
cat("最保守股票:", most_defensive, "Beta =", min(betas), "\n")

# (c)小題 異常報酬
alphas <- sapply(models, function(model) coef(model)[1])
alphas

alpha_pvalues <- sapply(models, function(model) summary(model)$coefficients[1,4])

significant_alpha <- names(alpha_pvalues[alpha_pvalues < 0.05])

cat("有異常報酬 的股票:", significant_alpha, "\n")

# (d)小題 Beta 的變異數與標準誤
beta_variances <- sapply(models, function(model) vcov(model)["MKT", "MKT"])
beta_se <- sqrt(beta_variances)

data.frame(Firm = firms, Beta = betas, Beta_Variance = beta_variances, Beta_SE = beta_se)
