if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("AER")
install.packages("car")
library(POE5Rdata)
data('capm5')
#(a) MSFT的beta
y <- capm5$msft - capm5$riskfree           # MSFT 超額報酬
x <- capm5$mkt - capm5$riskfree            # 市場超額報酬

data_capm <- data.frame(y = y, x = x)
model <- lm(y ~ x, data = data_capm)
summary(model)
#(b) RANK變數的first stage reg
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$RANK <- rank(capm5$mkt_excess, ties.method = "first")
# First-stage 回歸：以 RANK 解釋 mkt_excess
first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)
# (c)將first stage殘差納入回歸，觀察其係數
capm5$v <- residuals(first_stage)
capm5$y <- capm5$msft - capm5$riskfree
augmented_model <- lm(y ~ mkt_excess + v, data = capm5)
summary(augmented_model)
# (d) 2SLS RANK
library(AER)
iv_model <- ivreg(y ~ mkt_excess | RANK, data = capm5)
summary(iv_model)
# (e) 使用RANK、POS變數進行當作IV
capm5$POS <- ifelse(capm5$mkt > capm5$riskfree, 1, 0)
first_stage_2 <- lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage_2)
library(car)
linearHypothesis(first_stage_2, c("RANK = 0", "POS = 0"))
# (f) Hausmen Test
capm5$v_2 <- residuals(first_stage_2)
augmented_model_2 <- lm(y ~ mkt_excess + v_2, data = capm5)
summary(augmented_model_2, diagnostics = TRUE)
# (g) 2SLS POS+RANK
library(AER)
iv_model <- ivreg(y ~ mkt_excess | RANK + POS, data = capm5)
summary(iv_model)
# (h) Sargan Test
library(AER)
v_hat <- residuals(iv_model)

sargan_aux <- lm(v_hat ~ RANK + POS, data = capm5)
summary(sargan_aux)

n <- nobs(sargan_aux)                      # 樣本數
R2 <- summary(sargan_aux)$r.squared       # 輔助回歸的 R²
J_stat <- n * R2

df <- 2 - 1  # 工具變數數 - 內生變數數 = 1
p_value <- pchisq(J_stat, df = df, lower.tail = FALSE)

cat("Sargan J-statistic:", J_stat, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value, "\n")
