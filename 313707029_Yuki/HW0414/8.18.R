# 清除環境
rm(list=ls())

# 若尚未安裝，下列一行取消註解安裝
# install.packages(c("POE5Rdata","dplyr","ggplot2","sandwich","lmtest","nlme","car"))
library(POE5Rdata)   # cps5 資料
library(dplyr)
library(ggplot2)
library(sandwich)    # vcovHC()
library(lmtest)      # bptest(), coeftest()
library(nlme)        # gls()
library(car)         # for intervals()

data("cps5")

# 定義完整模型
model1 <- lm(wage ~ educ + exper + I(exper^2)
             + female + black + metro
             + south + midwest + west,
             data = cps5)

#------------------------------------------------
# (a) F 檢定：male vs. female 的誤差變異是否相同？
#  H0: σ²_M = σ²_F    H1: σ²_M ≠ σ²_F
#------------------------------------------------
m_m <- lm(wage ~ educ + exper + I(exper^2) + metro,
          data = filter(cps5, female == 0))
m_f <- lm(wage ~ educ + exper + I(exper^2) + metro,
          data = filter(cps5, female == 1))

SSE_m <- sum(resid(m_m)^2); df_m <- df.residual(m_m)
SSE_f <- sum(resid(m_f)^2); df_f <- df.residual(m_f)

F_a     <- (SSE_m/df_m) / (SSE_f/df_f)
Fcrit_a_low  <- qf(0.025, df1 = df_m, df2 = df_f)
Fcrit_a_high <- qf(0.975, df1 = df_m, df2 = df_f)

cat(" (a) H0: σ²_M = σ²_F vs. H1: σ²_M ≠ σ²_F\n")
cat(sprintf("     F = %.4f, rejection if F < %.4f or F > %.4f\n\n",
            F_a, Fcrit_a_low, Fcrit_a_high))

#------------------------------------------------
# (b) NR² Test (LM test)
#    H0: homoskedasticity   H1: heteroskedasticity
#------------------------------------------------
N <- nrow(cps5)
# (b1) 用 metro, female, black
aux1  <- lm(resid(model1)^2 ~ metro + female + black, data = cps5)
NR2_1 <- N * summary(aux1)$r.squared
crit1 <- qchisq(0.99, df = 3)   # 1% level, df = 3

cat(" (b1) NR² (metro,female,black):", round(NR2_1,4),
    " vs. χ²₀.₉₉,df=3 =", round(crit1,4), "\n")
cat("      Conclusion:",
    if(NR2_1>crit1) "Reject H0\n\n" else "Fail to reject H0\n\n")

# (b2) 用全部解釋變數 (9 個)
aux2  <- lm(resid(model1)^2 
            ~ educ + exper + I(exper^2)
            + female + black + metro
            + south + midwest + west,
            data = cps5)
NR2_2 <- N * summary(aux2)$r.squared
crit2 <- qchisq(0.99, df = 9)   # df = #vars = 9

cat(" (b2) NR² (all vars):", round(NR2_2,4),
    " vs. χ²₀.₉₉,df=9 =", round(crit2,4), "\n")
cat("      Conclusion:",
    if(NR2_2>crit2) "Reject H0\n\n" else "Fail to reject H0\n\n")

#------------------------------------------------
# (c) White Test
#    H0: homoskedasticity   H1: heteroskedasticity
#------------------------------------------------
# 同樣作 NR² 但自變數包含平方 & 交乘項
k0     <- 9
df_w   <- k0 + k0 + choose(k0,2)   # originals + squares + interactions
auxW   <- lm(resid(model1)^2 
             ~ (educ + exper + I(exper^2)
                + female + black + metro
                + south + midwest + west)^2,
             data = cps5)
White  <- N * summary(auxW)$r.squared
critW  <- qchisq(0.95, df = df_w)   # 5% level

cat(" (c) White test statistic =", round(White,2),
    ", df =", df_w,
    ", χ²₀.₉₅ =", round(critW,2), "\n")
cat("      Conclusion:",
    if(White>critW) "Reject H0\n\n" else "Fail to reject H0\n\n")

#------------------------------------------------
# (d) OLS + White robust SE：比較常規 vs. robust CI 寬度
#------------------------------------------------
# 常規 CI
ci_conv  <- confint(model1)

# robust CI
se_hc1   <- sqrt(diag(vcovHC(model1, type="HC1")))
t_d      <- qt(0.975, df.residual(model1))
ci_rob   <- cbind(
  Estimate = coef(model1),
  Lower95  = coef(model1) - t_d * se_hc1,
  Upper95  = coef(model1) + t_d * se_hc1
)

# 顯示某幾個係數的寬度變化
width_conv <- ci_conv[,2] - ci_conv[,1]
width_rob  <- ci_rob[rownames(ci_rob), "Upper95"] - ci_rob[rownames(ci_rob), "Lower95"]

cat(" (d) CI width change (robust vs. conv):\n")
for(nm in rownames(ci_rob)) {
  cat(sprintf("     %-10s: conv=%.4f, robust=%.4f → %s\n",
              nm,
              width_conv[nm],
              width_rob[nm],
              if(width_rob[nm]>width_conv[nm]) "wider" else "narrower"))
}
cat("\n")

#------------------------------------------------
# (e) FGLS (weights = exp(γ0 + γ1·metro + γ2·exper)):
#     以 metro+exper 作指數權重模型
#------------------------------------------------
fgls_mod <- gls(
  wage ~ educ + exper + I(exper^2)
  + female + black + metro
  + south + midwest + west,
  data    = cps5,
  weights = varExp(form = ~ metro + exper)
)

ci_all_conv <- intervals(fgls_mod, level = 0.95)$coef
ci_e_conv    <- ci_all_conv[c("exper","metro"), , drop = FALSE]
cat("=== (e) FGLS conventional 95% CI ===\n")
print(ci_e_conv)

#------------------------------------------------
# (f) FGLS + robust SE
#------------------------------------------------
# 1) 從 gls 物件提取已經計算好的權重向量 w_i = 1/Var(e_i)
wts <- weights(fgls_mod)

# 2) 用相同的 X, y 與 gls 模型，跑加權 lm
lm_fgls2 <- lm(
  wage ~ educ + exper + I(exper^2)
  + female + black + metro
  + south + midwest + west,
  data    = cps5,
  weights = wts
)

# 3) 對這個加權 lm 物件算 HC1 robust SE
vcov_fgls2_hc1 <- vcovHC(lm_fgls2, type = "HC1")
se_fgls2_rob   <- sqrt(diag(vcov_fgls2_hc1))

# 4) 取出 exper 與 metro 的 β̂
beta_f_exper <- coef(lm_fgls2)["exper"]
beta_f_metro <- coef(lm_fgls2)["metro"]

# 5) 計算自由度與 t 臨界值
df2   <- df.residual(lm_fgls2)            # = n - p
tcrit <- qt(0.975, df2)

# 6) 組出 95% robust CI
ci_fgls2_rob <- rbind(
  exper = beta_f_exper + c(-1, 1) * tcrit * se_fgls2_rob["exper"],
  metro = beta_f_metro +  c(-1, 1) * tcrit * se_fgls2_rob["metro"]
)
colnames(ci_fgls2_rob) <- c("Lower95","Upper95")

cat("=== (f) FGLS + robust‐SE 95% CI (使用 lm + weights) ===\n")
print(ci_fgls2_rob)
#------------------------------------------------
# (g) 報告建議
#------------------------------------------------
cat(" (g) 建議報告：OLS + White heteroskedasticity‐robust SE\n")
