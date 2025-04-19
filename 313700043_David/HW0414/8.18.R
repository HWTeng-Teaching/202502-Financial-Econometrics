rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
cps5

# a.

group1 <- cps5[cps5$female == 1, ]
group2 <- cps5[cps5$female == 0, ]

model1 <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west , data = group1)
model2 <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west , data = group2)
SSE1 <- sum(resid(model1)^2)
SSE2 <- sum(resid(model2)^2)

F_stat <- max(SSE1, SSE2) / min(SSE1, SSE2)
df1 <- model1$df.residual
df2 <- model2$df.residual
p_value1 <- pf(F_stat, df1, df2, lower.tail = FALSE)

cat("Goldfeld–Quandt Test 結果：\n")
cat("F 統計量 =", F_stat, "\n")
cat("自由度 =", df1, "和", df2, "\n")
cat("p 值 =", p_value, "\n")
if (p_value1 < 0.05) {
  cat("結論：拒絕 H0，存在異質變異（heteroskedasticity）\n")
} else {
  cat("結論：無法拒絕 H0，沒有足夠證據顯示存在異質變異\n")
}
# H0:模型不存在異質變異數 vs. H1:模型存在異質變異數

# b.

model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
resid <- resid(model)^2
aux_model <- lm(resid ~ metro + female + black, data = cps5)
NR2 <- nrow(cps5) * summary(aux_model)$r.squared
p_value2 <- pchisq(NR2, df = 3, lower.tail = FALSE)
cat("Test statistic (NR^2) =", NR2, "\n")
cat("p-value =", p_value2, "\n")
if (p_value2 < 0.01) {
  cat("結論：拒絕 H0，存在異質變異（heteroskedasticity）\n")
} else {
  cat("結論：無法拒絕 H0，沒有足夠證據顯示存在異質變異\n")
}
# H0:模型不存在異質變異數 vs. H1:模型存在異質變異數

aux_model2 <- lm(resid ~ educ + exper + I(exper^2) + metro + female + black + south + midwest + west, data = cps5)
NR2_2 <- nrow(cps5) * summary(aux_model2)$r.squared
p_value3 <- pchisq(NR2_2, df = 3, lower.tail = FALSE)
cat("Test statistic (NR^2) =", NR2_2, "\n")
cat("p-value =", p_value3, "\n")
if (p_value3 < 0.01) {
  cat("結論：拒絕 H0，存在異質變異（heteroskedasticity）\n")
} else {
  cat("結論：無法拒絕 H0，沒有足夠證據顯示存在異質變異\n")
}
# H0:模型不存在異質變異數 vs. H1:模型存在異質變異數

# c

bptest(model, data = cps5)
# 拒絕虛無假設，有證據支持誤差項與解釋變數有關

# d

summary(model)
ci_conven <- round(confint(model, level = 0.95), digits = 4)
ci_conven

cov <- hccm(model, type = "hc1")
vcvmodel <- coeftest(model, vcov = cov)
vcvmodel
ci_robust <- round(confint(vcvmodel, level = 0.95), digits = 4)
ci_robust
                        
comci <- data.frame(ci_conven, ci_robust)
colnames(comci) <- c("2.5% (con)", "97.5% (con)",
                     "2.5% (robust)", "97.5% (robust)")
comci
# narrower:femle, black, metro, midwest
# wider:intercept, educ, exper, south, west
# same: exper^2

# e

model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
resid <- resid(model)^2
residhatsq.ols <- lm(log(resid) ~ exper + metro , data = cps5)
vari <- exp(fitted(residhatsq.ols))
cps5fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
               weights = 1/vari,  data = cps5)

summary(cps5fgls)
ci_fgls <- round(confint(cps5fgls, level = 0.95), digits = 4)
comci2 <- data.frame(ci_robust, ci_fgls)
colnames(comci2) <- c("2.5% (robust)", "97.5% (robust)",
                     "2.5% (fgls)", "97.5% (fgls)")
comci2
# narrower:metro,intercept,  educ, exper, south, west
# wider:black, midwest 
# same: exper^2, femle

# f

fgls_model <- gls(
  log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
  data = cps5,
  weights = varComb(
    varExp(~ exper,value = 0.1),           # 更穩定的連續變數異變結構
    varIdent(form = ~ 1 | metro)          # 地區類別變異結構
  )
)
robust_cov <- vcov(fgls_model, type = "HC1")
robust_fgls_result <- coeftest(fgls_model, vcov = robust_cov)
ci_fgls_robust <- confint(fgls_model, level = 0.95)

comci3 <- data.frame(ci_robust, ci_fgls, ci_fgls_robust)
colnames(comci3) <- c("2.5% (robust)", "97.5% (robust)",
                      "2.5% (fgls)", "97.5% (fgls)",
                      "2.5%(fgls_robust)", "97.5%(fgls_robust)")
comci3

# g.

# 從各模型的係數信賴區間來看，傳統 GLS 的信賴區間估計有四個是最窄的，因此模型可能比較好。




