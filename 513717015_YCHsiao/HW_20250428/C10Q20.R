
# 載入套件與資料
library(POE5Rdata)
data("capm5")

# 確認欄位
names(capm5)

# 建立超額報酬變數（正確欄位名稱）
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt  - capm5$riskfree

# OLS：估計 CAPM 模型
ols_model <- lm(excess_msft ~ excess_mkt, data = capm5)

# 查看回歸結果
summary(ols_model)

##############b #############

# 重新確認資料
library(POE5Rdata)
data("capm5")
capm5$excess_mkt <- capm5$mkt - capm5$riskfree

# 建立 RANK 工具變數：對 excess_mkt 由小到大排序
capm5$rank <- rank(capm5$excess_mkt)

# 第一階段回歸：excess_mkt ~ rank
first_stage <- lm(excess_mkt ~ rank, data = capm5)

# 顯示回歸摘要
summary(first_stage)





####################c ##############

# 載入必要套件
library(AER)

# 建立超額報酬（如尚未執行）
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt  <- capm5$mkt  - capm5$riskfree
capm5$rank        <- rank(capm5$excess_mkt)

# 執行 IV/2SLS 模型
iv_model <- ivreg(excess_msft ~ excess_mkt | rank, data = capm5)

# 顯示結果
summary(iv_model)





#####################e ###################

# 確保你已載入以下套件
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

# 如果還沒有建立 OLS 模型，可以重新估計一次
ols_model <- lm(excess_msft ~ excess_mkt, data = capm5)

# 使用 robust 標準誤（White HC1）
robust_ols <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC1"))

# 顯示結果
print(robust_ols)


################f ################

# 1. 第一階段：用工具變數 rank 預測 excess_mkt，並取得殘差
first_stage <- lm(excess_mkt ~ rank, data = capm5)
capm5$fs_resid <- resid(first_stage)

# 2. 第二階段：將第一階段殘差加進 OLS 模型
hausman_test <- lm(excess_msft ~ excess_mkt + fs_resid, data = capm5)

# 3. 查看 fs_resid 是否顯著（t-test p-value < 0.01）
summary(hausman_test)


################g ##################

# 建立 POS 變數（當 excess_mkt > 0 時為 1，否則為 0）
capm5$pos <- ifelse(capm5$excess_mkt > 0, 1, 0)

# 第一階段回歸：將 rank 與 pos 一起作為解釋變數
first_stage_g <- lm(excess_mkt ~ rank + pos, data = capm5)

# 顯示回歸結果
summary(first_stage_g)


##################h ####################

# 1. 先取得模型的殘差
iv_resid <- resid(iv_model_h)

# 2. 建立工具變數矩陣（rank 和 pos）
Z <- as.matrix(capm5[, c("rank", "pos")])

# 3. 計算 J 統計量（Hansen's J = n × R² from regression of residuals on IVs）
j_test_model <- lm(iv_resid ~ Z)
j_r2 <- summary(j_test_model)$r.squared
n <- nrow(capm5)
j_stat <- n * j_r2

# 4. p-value（自由度 = 工具變數數 - 內生變數數 = 2 - 1 = 1）
p_value <- 1 - pchisq(j_stat, df = 1)

# 5. 顯示結果
cat("Hansen J statistic =", round(j_stat, 4), "\n")
cat("p-value =", round(p_value, 5), "\n")



