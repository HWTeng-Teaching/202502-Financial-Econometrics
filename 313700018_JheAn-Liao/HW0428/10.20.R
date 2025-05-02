
rm(list=ls())
library(AER)
summary(iv_msft, diagnostics = TRUE)
# 🔗 下載並載入 vacation 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(capm5)

#A
#建立風險溢籌
capm5 <- transform(capm5,
                   er_msft =  msft - riskfree,
                   er_mkt  = mkt - riskfree
                   )


capm_msft <- lm(er_msft ~ er_mkt, data = capm5)
summary(capm_msft)

# 取出 β 及 95% 信賴區間
beta_hat   <- coef(capm_msft)["er_mkt"]
se_beta    <- summary(capm_msft)$coef["er_mkt", "Std. Error"]
ci_beta    <- confint(capm_msft)["er_mkt", ]
list(beta = beta_hat,
     se   = se_beta,
     ci95 = ci_beta)
#系統風險較高

#B
# -----------------------------------------------------------
# 1. 建立「市場風險溢酬」與其排名 (RANK)
# -----------------------------------------------------------
capm5 <- transform(capm5,
                   er_mkt = mkt - riskfree,                 # 解釋變數（可能帶量測誤）
                   RANK   = rank(er_mkt, ties.method = "first")   # 1~180 由小排到大
)

# -----------------------------------------------------------
# 2. 第一階段：er_mkt ~ RANK
# -----------------------------------------------------------
first_capm <- lm(er_mkt ~ RANK, data = capm5)
summary(first_capm)

# F-檢定 (與 summary 報表吻合)
fs_F <- summary(first_capm)$fstatistic
fs_F

#滿足IV1 - IV3
#F值 = 94 屬於強工具

#C
#------------------------------------------------------------
# 第一階段：er_mkt ~ RANK
#------------------------------------------------------------
fs_fit  <- lm(er_mkt ~ RANK, data = capm5)

# 取出殘差 û_t
capm5$uhat <- resid(fs_fit)
#------------------------------------------------------------
# 增廣 OLS：是否需要 IV?
#------------------------------------------------------------
aug_fit <- lm(er_msft ~ er_mkt + uhat, data = capm5)
sum_aug <- summary(aug_fit)
sum_aug
#在0.05水準下顯著，拒絕外生性假設

#D 把RANK 當作 市場超額報酬的工具變數
capm5 <- transform(capm5,
                   er_msft = msft - riskfree,
                   er_mkt  = mkt  - riskfree,
                   RANK    = rank(er_mkt, ties.method = "first")
                   )

#2SLS
iv_msft <- ivreg(er_msft ~ er_mkt |
                   RANK, data = capm5)

summary(iv_msft)
confint(iv_msft)["er_mkt", ]               # 95% CI

# beta hat > 1 依舊為進攻型股票，且係數比OLS更大

#E
capm5 <- transform(capm5,
                   er_mkt = mkt - riskfree,
                   er_msft = msft - riskfree,
                   RANK = rank(er_mkt, ties.method = "first"),
                   POS = as.integer(er_mkt >0 )
)
#第一階對迴歸 估計超額報酬
first2 <- lm(er_mkt ~ RANK + POS, data = capm5)
summary(first2)
#檢定是否為強ＩＶ
joint <- linearHypothesis(first2,
                          c("RANK = 0",
                            "POS = 0"))
print(joint)

# F value = 951 -> 強工具 且R2 = 0.91 此兩工具的組合能夠解釋大部分的內生解釋變數的變異

#F
capm5$uhat <- resid(first2)
aug <- lm(er_msft ~ er_mkt + uhat , data = capm5)
summary(aug)
# p value = 0.0287 > 0.01 不拒絕 H0 可以視超額市場報酬為外生變數


#G
# 2. OLS 基準估計
ols_fit  <- lm(er_msft ~ er_mkt, data = capm5)
beta_ols <- coef(ols_fit)["er_mkt"]
se_ols   <- summary(ols_fit)$coef["er_mkt","Std. Error"]
ci_ols   <- confint(ols_fit)["er_mkt", ]

# 3. IV/2SLS：工具 = {RANK, POS}
iv2_fit  <- ivreg(er_msft ~ er_mkt | RANK + POS, data = capm5)
sum_iv2  <- summary(iv2_fit)
beta_iv2 <- coef(iv2_fit)["er_mkt"]
se_iv2   <- sum_iv2$coefficients["er_mkt","Std. Error"]
ci_iv2   <- confint(iv2_fit)["er_mkt", ]

# 4. 列印比較結果
cat("---- OLS ----\n")
cat(sprintf("β̂_OLS = %.4f, SE = %.4f, 95%% CI = [%.4f, %.4f]\n\n",
            beta_ols, se_ols, ci_ols[1], ci_ols[2]))

cat("---- IV/2SLS (RANK + POS) ----\n")
cat(sprintf("β̂_IV  = %.4f, SE = %.4f, 95%% CI = [%.4f, %.4f]\n",
            beta_iv2, se_iv2, ci_iv2[1], ci_iv2[2]))

#工具變數的估計係數較大，符合ＯＬＳ衰減偏誤的假設

#H
# 1. 取出 2SLS 殘差 û_iv
res_iv <- resid(iv2_fit)

# 2. 用所有工具做迴歸： û_iv ~ RANK + POS
aux <- lm(res_iv ~ RANK + POS, data = capm5)

# 3. 讀出 R² 及樣本數
R2_aux <- summary(aux)$r.squared
n      <- length(res_iv)

# 4. 計算 Sargan 統計量： n * R²
Sargan_stat <- n * R2_aux

# 5. p 值與臨界值（df = (#Z − #endog) = 1）
p_value    <- 1 - pchisq(Sargan_stat, df = 1)
crit_value <- qchisq(0.95, df = 1)  # ≈ 3.841

# 6. 輸出結果
cat(sprintf("Sargan 統計量 = %.3f\n", Sargan_stat))
cat(sprintf("χ²(1) 臨界值 (5%%) = %.3f\n", crit_value))
cat(sprintf("p-value = %.3f\n", p_value))

if (Sargan_stat > crit_value) {
  cat("結論：拒絕 H0，工具疑似非外生給定。\n")
} else {
  cat("結論：無法拒絕 H0，工具為外生之假設在 5% 水準下成立。\n")
}

