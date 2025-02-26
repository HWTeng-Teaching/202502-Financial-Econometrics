
temp_file <- tempfile(fileext = ".rdata")

# 下载文件
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata", 
              destfile = temp_file, 
              mode = "wb")  # 二进制模式下载

# 加载数据
load(temp_file)
capm5


# 确定正确的列名(如果需要调整请根据names(capm5)的输出结果修改)
firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")
market_col <- "mkt"  # 市场回报率的列名
riskfree_col <- "riskfree"  # 无风险利率的列名

# 计算超额市场回报
capm5$MKT_excess <- capm5[[market_col]] - capm5[[riskfree_col]]

# 创建数据框存储结果
results <- data.frame(Firm = firms, Beta = numeric(length(firms)))

# 对每家公司估计CAPM模型
for(i in 1:length(firms)) {
  # 计算公司超额收益
  firm_excess <- capm5[[firms[i]]] - capm5[[riskfree_col]]
  
  # 估计CAPM模型: (R_i - R_f) = α + β(R_m - R_f) + ε
  model <- lm(firm_excess ~ capm5$MKT_excess)
  
  # 存储beta值
  results$Alpha[i] <- coef(model)[1]
  results$Beta[i] <- coef(model)[2]
}

# 输出beta值结果
print(results)

# 找出最具防御性和进攻性的公司
defensive_firm <- results$Firm[which.min(results$Beta)]
defensive_beta <- min(results$Beta)

aggressive_firm <- results$Firm[which.max(results$Beta)]
aggressive_beta <- max(results$Beta)

# 打印结果
cat(defensive_firm, "Beta值 =", round(defensive_beta, 4), "\n")
cat(aggressive_firm, "Beta值 =", round(aggressive_beta, 4), "\n")

# 計算 Microsoft 的超額報酬
msft_excess <- capm5$msft - capm5$riskfree

# 取出市場超額報酬
market_excess <- capm5$MKT_excess

# 建立線性回歸模型
model_msft <- lm(msft_excess ~ market_excess)

# 繪製散點圖
plot(market_excess, msft_excess,
     main = "CAPM for Microsoft",
     xlab = "Market Excess Return (R_m - R_f)",
     ylab = "MSFT Excess Return (R_MSFT - R_f)",
     pch = 19,       # 點的形狀
     col = "blue")   # 點的顏色
# 加入回歸直線
abline(model_msft, col = "red", lwd = 2)


library(ggplot2)
alpha_msft <- results$Alpha[4]
beta_msft <- results$Beta[4]
print(alpha_msft)
print(beta_msft)
# 計算 Microsoft 的超額報酬（如果尚未在資料框內）
capm5$msft_excess <- capm5$msft - capm5$riskfree

ggplot(capm5, aes(x = MKT_excess, y = msft_excess)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    x = expression(R[m] - R[f]),
    y = expression(R[MSFT] - R[f]),
    title = bquote(
      "CAPM for Microsoft: " ~ (R[MSFT] - R[f]) == .(round(alpha_msft,4))
      + .(round(beta_msft,4)) * (R[m] - R[f])
    )
  )

results_no_intercept <- data.frame(
  Firm = firms,
  Beta = numeric(length(firms))  # 預先放置 Beta (無截距) 的欄位
)
for (i in seq_along(firms)) {
  
  # 公司超額報酬 (R_i - R_f)
  firm_excess <- capm5[[firms[i]]] - capm5$riskfree
  
  # 無截距迴歸：(R_i - R_f) = β(R_m - R_f)
  # 注意 formula 寫法：firm_excess ~ 0 + capm5$MKT_excess
  model_no_intercept <- lm(firm_excess ~ 0 + capm5$MKT_excess)
  
  # 提取 β，因為模型只有一個係數，所以 coef(model_no_intercept)[1] 即 β
  results_no_intercept$Beta[i] <- coef(model_no_intercept)[1]
}

# 檢視結果
print(results_no_intercept)

# 新建 results，其中放四個欄位：
#  - Beta_with_intercept: 有截距CAPM的Beta
#  - Beta_no_intercept: 無截距CAPM的Beta
#  - Alpha: 有截距CAPM中的Alpha
#  - Firm: 公司名稱
results <- data.frame(
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
  results$Alpha[i]              <- coef(model_with_int)[1]
  results$Beta_with_intercept[i] = coef(model_with_int)[2]
  
  ## (B) 無截距CAPM
  # (R_i - R_f) = β (R_m - R_f)
  model_no_int <- lm(firm_excess ~ 0 + capm5$MKT_excess)
  
  # 無截距模型只有1個係數，存為 Beta
  results$Beta_no_intercept[i] = coef(model_no_int)[1]
}

# 查看結果
results$Beta_diff <- results$Beta_with_intercept - results$Beta_no_intercept
print(results)
