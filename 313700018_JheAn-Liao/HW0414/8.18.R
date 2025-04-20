rm(list=ls()) # Caution: this clears the Environment
library(lmtest)
library(sandwich)
# 🔗 下載並載入 cps5 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(cps5)

##A#####
#分群
m <- subset(cps5, female == 0)
w <- subset(cps5, female == 1)

fm <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = m)
ff <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west, data = w)
s2_m <- sum(resid(fm)^2)/fm$df.residual
s2_w <- sum(resid(fm)^2)/ff$df.residual
df_m <- fm$df.residual
df_w <- ff$df.residual

Fstat <- if(s2_m / s2_w >1) s2_m/s2_w else s2_w/s2_m
alpha <- 0.05
F_low  <- qf(alpha/2,  df1=min(df_m,df_w), df2=max(df_m,df_w))
F_high <- qf(1-alpha/2, df1=min(df_m,df_w), df2=max(df_m,df_w))

# 6. 印出結果並判斷
cat("F 統計量 =", round(Fstat,3), 
    "  臨界區間 = (", round(F_low,3), ",", round(F_high,3), ")\n")

if (Fstat < F_low || Fstat > F_high) {
  cat("→ 拒絕 H0：男／女組誤差變異數不同（有異質變異數）。\n")
} else {
  cat("→ 無法拒絕 H0：男／女組誤差變異數相等（無顯著異質變異數）。\n")
}

##B#####
model <- lm(log(wage) ~ educ + exper + I(exper^2)
            + female + black
            + metro + south + midwest + west,
            data = cps5)
res2 <- resid(model)^2
aux1 <- lm(res2 ~ metro + female +black,
           data = cps5)
n <- nrow(cps5)
r2 <- summary(aux1)$r.squared
stat <- n * r2
df1 <- 3 
p1    <- 1 - pchisq(stat, df1)
cat("NR² 統計量 =", round(stat,3),
    "  df =", df1,
    "  p‑value =", round(p1,4), "\n")
if (p1 < 0.01) cat("→ 在 1% 水準下拒絕 H0，存在異質變異數。\n")
else           cat("→ 無法拒絕 H0，未見異質變異數。\n")

# 4. (ii) 把所有解釋變數都納入候選
aux2 <- lm(res2 ~ educ + exper + I(exper^2)
           + female + black
           + metro + south + midwest + west,
           data = cps5)
R2_2  <- summary(aux2)$r.squared
LM2   <- n * R2_2
df2   <- length(coef(aux2)) - 1  # 候選變數個數
p2    <- 1 - pchisq(LM2, df2)

cat("NR² 統計量 =", round(LM2,3),
    "  df =", df2,
    "  p‑value =", round(p2,4), "\n")
if (p2 < 0.01) cat("→ 在 1% 水準下拒絕 H0，存在異質變異數。\n")
else           cat("→ 無法拒絕 H0，未見異質變異數。\n")

##C#######
aux <- lm(resid(model)^2 ~ educ + I(educ^2)
          + exper + I(exper^2)
          + female + black
          + metro + south + midwest + west,
          data = cps5)

n    <- nrow(cps5)         
R2   <- summary(aux)$r.squared 
LM   <- n * R2                    
df   <- length(coef(aux)) - 1    
crit <- qchisq(0.95, df = df)      

cat("LM =", round(LM,2),
    "  df =", df,
    "  χ²_0.95 =", round(crit,2), "\n")
##D##########
# (1) 估 OLS 主模型
model <- lm(log(wage) ~ educ + exper + I(exper^2)
            + female + black
            + metro + south + midwest + west,
            data = cps5)

# (2) 傳統 95% CI
ci_conv <- confint(model, level = 0.95)

# (3) White HC1 robust covariance
vcov_hc1  <- vcovHC(model, type = "HC1")
# robust 95% CI（手動計算）
beta      <- coef(model)
se_robust <- sqrt(diag(vcov_hc1))
tcrit     <- qt(0.975, df = model$df.residual)
ci_robust <- cbind(
  beta - tcrit * se_robust,
  beta + tcrit * se_robust
)
rownames(ci_robust) <- names(beta)
colnames(ci_robust) <- c("2.5%", "97.5%")

# (4) 比較寬度
width_conv  <- ci_conv[,2] - ci_conv[,1]
width_robust<- ci_robust[,2] - ci_robust[,1]
compare <- data.frame(
  Estimate      = coef(model),
  Conv_Lower    = ci_conv[,1],
  Conv_Upper    = ci_conv[,2],
  Robust_Lower  = ci_robust[,"2.5%"],
  Robust_Upper  = ci_robust[,"97.5%"],
  Width_Conv    = width_conv,
  Width_Robust  = width_robust,
  Change        = width_robust - width_conv
)
print(compare)

##E############
ols <- lm(log(wage) ~ educ + exper + I(exper^2)
          + female + black
          + metro + south + midwest + west,
          data = cps5)
e2  <- resid(ols)^2
sked <- lm(log(e2) ~ metro + exper, data = cps5)
hi   <- exp(predict(sked))
fgls <- lm(log(wage) ~ educ + exper + I(exper^2)
           + female + black
           + metro + south + midwest + west,
           data = cps5,
           weights = 1/hi)
ci_fgls <- confint(fgls, level = 0.95)
ci_fgls

##F#####
# 1. OLS 估計並取殘差平方
ols <- lm(log(wage) ~ educ + exper + I(exper^2)
          + female + black
          + metro + south + midwest + west,
          data = cps5)
e2  <- resid(ols)^2

# 2. 擬合異質變異數函數 h_i = exp(alpha1 + alpha2*metro + alpha3*exper)
sked  <- lm(log(e2) ~ metro + exper, data = cps5)
h_i   <- exp(predict(sked))       # 估計出的 h_i

# 3. 傳統 FGLS（WLS）估計：權重 w_i = 1/h_i
fgls  <- lm(log(wage) ~ educ + exper + I(exper^2)
            + female + black
            + metro + south + midwest + west,
            data = cps5,
            weights = 1/h_i)

# (e) 傳統 FGLS 的 95% 信賴區間
ci_fgls <- confint(fgls, level = 0.95)

# 4. FGLS + HC1 robust 標準誤
vcov_fgls_hc1 <- vcovHC(fgls, type = "HC1")
beta          <- coef(fgls)
se_fgls_rob   <- sqrt(diag(vcov_fgls_hc1))
tcrit         <- qt(0.975, df = fgls$df.residual)
ci_fgls_rob   <- cbind(
  beta - tcrit * se_fgls_rob,
  beta + tcrit * se_fgls_rob
)
colnames(ci_fgls_rob) <- c("2.5%", "97.5%")

# 5. OLS + HC1 robust（(d) 的結果，用來比較）
vcov_ols_hc1  <- vcovHC(ols, type = "HC1")
beta_ols      <- coef(ols)
se_ols_rob    <- sqrt(diag(vcov_ols_hc1))
ci_ols_rob    <- cbind(
  beta_ols - tcrit * se_ols_rob,
  beta_ols + tcrit * se_ols_rob
)
colnames(ci_ols_rob) <- c("2.5%", "97.5%")

# 6. 匯總比較
methods <- c("OLS+HC1", "FGLS", "FGLS+HC1")
ci_mat  <- rbind(ci_ols_rob, ci_fgls["kids",], ci_fgls_rob["kids",])
widths  <- ci_mat[,2] - ci_mat[,1]
compare <- data.frame(
  Method = methods,
  Lower  = ci_mat[,1],
  Upper  = ci_mat[,2],
  Width  = widths
)
print(compare)

##G#######
#在真正發表時，我會採用 OLS + heteroskedasticity‑robust 標準誤（也就是 (d) 的那一套結果）

