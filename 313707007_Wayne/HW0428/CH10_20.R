#a

# 載入資料
capm <- read.csv("capm5.csv")

# 計算超額報酬
capm$excess_msft <- capm$msft - capm$riskfree
capm$excess_mkt <- capm$mkt - capm$riskfree

# OLS 回歸
model_ols <- lm(excess_msft ~ excess_mkt, data = capm)

# 顯示摘要
summary(model_ols)


#b
# 載入資料
capm <- read.csv("capm5.csv")

# 建立超額報酬變數
capm$excess_mkt <- capm$mkt - capm$riskfree
capm$excess_msft <- capm$msft - capm$riskfree

# 建立 RANK（根據 excess_mkt 排序）
capm$RANK <- rank(capm$excess_mkt, ties.method = "first")

# 第一階段回歸：excess_mkt ~ RANK
first_stage <- lm(excess_mkt ~ RANK, data = capm)

# 顯示結果
summary(first_stage)

#c
# 第一步：第一階段回歸（已完成）
first_stage <- lm(excess_mkt ~ RANK, data = capm)

# 第二步：取得殘差（v hat）
capm$vhat <- resid(first_stage)

# 第三步：進行擴充後的 CAPM 模型回歸
augmented_model <- lm(excess_msft ~ excess_mkt + vhat, data = capm)

# 顯示回歸摘要
summary(augmented_model)


#d
# Create the instrumental variable RANK by ranking the market excess returns
# from smallest to largest. Ties are assigned based on order of appearance.
capm$RANK <- rank(capm$excess_mkt, ties.method = "first")
first_stage <- lm(excess_mkt ~ RANK, data = capm)
summary(first_stage)
library(AER)
iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm)
summary(iv_model)



#e
# 建立 POS 變數
capm$POS <- ifelse(capm$excess_mkt > 0, 1, 0)

# 第一階段回歸：excess_mkt ~ RANK + POS
first_stage2 <- lm(excess_mkt ~ RANK + POS, data = capm)

# 顯示摘要
summary(first_stage2)

#f
# 第一步：取得第一階段殘差（用 RANK + POS 為工具）
first_stage2 <- lm(excess_mkt ~ RANK + POS, data = capm)
capm$vhat2 <- resid(first_stage2)  # 這是新的殘差

# 第二步：將 vhat2 加入到 CAPM 模型中
hausman_model <- lm(excess_msft ~ excess_mkt + vhat2, data = capm)

# 顯示結果
summary(hausman_model)


#g
library(AER)

# 使用兩個 IV：RANK 和 POS 進行 2SLS
iv_model2 <- ivreg(excess_msft ~ excess_mkt | RANK + POS, data = capm)

# 顯示估計結果
summary(iv_model2)

 


