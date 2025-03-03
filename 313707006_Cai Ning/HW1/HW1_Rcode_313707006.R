#2.16(b)

library(POE5Rdata)
data("capm5")
colnames(capm5)

library(POE5Rdata)
data("capm5")
colnames(capm5)

#計算超額回報
capm5$ge_execss <- capm5$ge - capm5$riskfree
capm5$ibm_execss <- capm5$ibm - capm5$riskfree
capm5$ford_execss <- capm5$ford - capm5$riskfree
capm5$msft_execss <- capm5$msft - capm5$riskfree
capm5$dis_execss <- capm5$dis - capm5$riskfree
capm5$xom_execss <- capm5$xom - capm5$riskfree
capm5$mkt_execss <- capm5$mkt - capm5$riskfree

#建立儲存結果的 list
model_summaries <- list()
beta_values <- c()
intercept_values <- c()

#迴圈跑回歸模型
for (company in c("ge_execss", "ibm_execss", "ford_execss", "msft_execss", "dis_execss", "xom_execss")) 
{
  model <- lm(as.formula(paste(company, "~ mkt_execss")), data = capm5)
  
  # 儲存完整回歸模型摘要
  model_summaries[[company]] <- summary(model)
  
  # 儲存係數
  beta_values[company] <- coef(model)[2]
  intercept_values[company] <- coef(model)[1]
}

# 印出所有模型摘要
for (company in names(model_summaries)) 
{
  cat("\n回歸結果:", company, "\n")
  print(model_summaries[[company]])
}

#2.16(C)

#公司截距項檢定
t_stat_values <- c()
p_values <- c()
for (company in c("ge_execss", "ibm_execss", "ford_execss", "msft_execss", "dis_execss", "xom_execss")) 
{
  model <- lm(as.formula(paste(company, "~ mkt_execss")), data = capm5)
  coef_values <- coef(model)
  t_stat_values[company] <- summary(model)$coefficients[1, 3]  # 截距的 t 統計量
  p_values[company] <- summary(model)$coefficients[1, 4]  # 截距的 p 值
}

#印出結果
data.frame(
  公司 = c("GE", "IBM", "Ford", "MSFT", "DIS", "XOM"),
  intercept_values,
  intercept_T = t_stat_values,
  intercept_P = p_values,
  beta_values
)

#散佈圖
plot(capm5$mkt_execss, capm5$msft_execss, 
     main = "Scatter plot of msft_execss vs. mkt_execss", 
     xlab = "Market Excess Returns", 
     ylab = "Microsoft Excess Returns", 
     pch = 19, col = "blue")
model_msft <- lm(msft_execss ~ mkt_execss, data = capm5)
abline(model_msft, col = "red", lwd = 2)  # 加上擬合線

#2.16(d)

beta_values_zero_intercept <- c()
for (company in c("ge_execss", "ibm_execss", "ford_execss", "msft_execss", "dis_execss", "xom_execss")) 
{
  # 強制截距為零的回歸模型
  model_zero_intercept <- lm(as.formula(paste(company, "~ mkt_execss - 1")), data = capm5)  # -1 用來排除截距
  beta_values_zero_intercept[company] <- coef(model_zero_intercept)[1]  # 由於沒有截距，beta 是第一個係數
}
beta_comparison <- data.frame(
  公司 = c("GE", "IBM", "Ford", "MSFT", "DIS", "XOM"),
  ori_Beta = beta_values,
  alpha_0_Beta = beta_values_zero_intercept
)

print(beta_comparison)
