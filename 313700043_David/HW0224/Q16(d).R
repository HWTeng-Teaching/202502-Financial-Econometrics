# 新建 results，其中放四個欄位：
#  - Beta_with_intercept: 有截距CAPM的Beta
#  - Beta_no_intercept: 無截距CAPM的Beta
#  - Alpha: 有截距CAPM中的Alpha
#  - Firm: 公司名稱
results_compare <- data.frame(
  Firm = firms,
  Alpha = numeric(length(firms)),
  Beta_with_intercept = numeric(length(firms)),
  Beta_no_intercept   = numeric(length(firms)),
  stringsAsFactors = FALSE
)

for (i in seq_along(firms)) {
  
  # 1. 取得公司超額報酬
  firm_excess <- capm5[[firms[i]]] - capm5$riskfree
  
  ## (A) 有截距CAPM
  # (R_i - R_f) = α + β (R_m - R_f)
  model_with_int <- lm(firm_excess ~ capm5$MKT_excess)
  
  # 截距(Alpha) & Beta
  results_compare$Alpha[i]              <- coef(model_with_int)[1]
  results_compare$Beta_with_intercept[i] = coef(model_with_int)[2]
  
  ## (B) 無截距CAPM
  # (R_i - R_f) = β (R_m - R_f)
  model_no_int <- lm(firm_excess ~ 0 + capm5$MKT_excess)
  
  # 無截距模型只有1個係數，存為 Beta
  results_compare$Beta_no_intercept[i] = coef(model_no_int)[1]
}

# 查看結果
results_compare$Beta_diff <- results_compare$Beta_with_intercept - results_compare$Beta_no_intercept
print(results_compare)
