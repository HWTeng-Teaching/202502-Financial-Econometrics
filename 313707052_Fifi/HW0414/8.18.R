library(POE5Rdata)
data("cps5", package = "POE5Rdata")

names(cps5)

#a.
library(broom)

alpha <- 0.05

male <- cps5[cps5$female == 0, ]
female <- cps5[cps5$female == 1, ]

model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + black + south + midwest + west, data = male)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + black + south + midwest + west, data = female)

df_male <- model_male$df.residual
df_female <- model_female$df.residual

sigma2_male <- glance(model_male)$sigma^2
sigma2_female <- glance(model_female)$sigma^2

fstat <- sigma2_female / sigma2_male
F_lower <- qf(alpha/2, df_female, df_male)
F_upper <- qf(1 - alpha/2, df_female, df_male)

cat("F lower critical value =", F_lower, "\n")
cat("F upper critical value =", F_upper, "\n")
cat("F statistic =", fstat, "\n")

if (fstat < F_lower | fstat > F_upper) {
  cat("Reject H0: Evidence of unequal variances.\n")
} else {
  cat("Fail to reject H0: No evidence of unequal variances.\n")
}

#b.

model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
summary(model_ols)
e2 <- resid(model_ols)^2

aux1 <- lm(e2 ~ metro + female + black, data = cps5)

N <- nrow(cps5)
R2_1 <- summary(aux1)$r.squared
LM1 <- N * R2_1

pval1 <- 1 - pchisq(LM1, df = 3)
cat("Test 1 (metro, female, black):\n")
cat("NR² =", LM1, "\n")
cat("p-value =", pval1, "\n")

aux2 <- lm(e2 ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

R2_2 <- summary(aux2)$r.squared
LM2 <- N * R2_2
pval2 <- 1 - pchisq(LM2, df = 9)  # 有 9 個變數

cat("\nTest 2 (all RHS variables):\n")
cat("NR² =", LM2, "\n")
cat("p-value =", pval2, "\n")

#c.

# Step 1: OLS 模型
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
e2 <- resid(model_ols)^2
N <- nrow(cps5)

# Step 2: 生成平方與交互項（連續 × 虛擬）
cps5$educ2 <- cps5$educ^2
cps5$exper2 <- cps5$exper^2

# 連續 × 虛擬
cps5$educ_female <- cps5$educ * cps5$female
cps5$educ_black <- cps5$educ * cps5$black
cps5$educ_metro <- cps5$educ * cps5$metro
cps5$educ_south <- cps5$educ * cps5$south
cps5$educ_midwest <- cps5$educ * cps5$midwest
cps5$educ_west <- cps5$educ * cps5$west

cps5$exper_female <- cps5$exper * cps5$female
cps5$exper_black <- cps5$exper * cps5$black
cps5$exper_metro <- cps5$exper * cps5$metro
cps5$exper_south <- cps5$exper * cps5$south
cps5$exper_midwest <- cps5$exper * cps5$midwest
cps5$exper_west <- cps5$exper * cps5$west

# Step 3: 輔助迴歸
aux_white <- lm(e2 ~ educ + exper + educ2 + exper2 +
                  female + black + metro + south + midwest + west +
                  educ_female + educ_black + educ_metro + educ_south + educ_midwest + educ_west +
                  exper_female + exper_black + exper_metro + exper_south + exper_midwest + exper_west,
                data = cps5)

# Step 4: 統計計算
R2 <- summary(aux_white)$r.squared
LM <- N * R2
df_white <- length(coef(aux_white)) - 1
crit <- qchisq(0.95, df_white)
pval <- 1 - pchisq(LM, df_white)

# Step 5: 輸出
cat("White test (with interaction terms):\n")
cat("NR² =", LM, "\n")
cat("df =", df_white, "\n")
cat("5% critical value =", crit, "\n")
cat("p-value =", pval, "\n")

if (LM > crit) {
  cat("Reject H0: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject H0: No evidence of heteroskedasticity.\n")
}
