# 執行無截距回歸
capm_no_intercept <- lapply(stock_cols, function(stock) {
  lm(as.formula(paste0(stock, "_rf ~ 0 + mkt_rf")), data = capm5)
})

# 提取新的 Beta 值
beta_values_no_intercept <- data.frame(
  Company = stock_cols,
  Beta = sapply(capm_no_intercept, function(model) coef(model)[1])
)

# 顯示 Beta 值
print("📊 Beta 值（無截距模型）：")
print(beta_values_no_intercept)

