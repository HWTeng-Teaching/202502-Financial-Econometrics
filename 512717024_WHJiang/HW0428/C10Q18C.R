# C10Q18 Instrumental Variables

##############################################################################
# 前置設定：清除環境、載入套件與資料
##############################################################################
rm(list = ls())  # 清除工作環境

### 載入必要套件
## 套件與資料 ------------------------------------------------------------
if (!require("AER"))        install.packages("AER")
if (!require("ivreg"))      install.packages("ivreg")
if (!require("lmtest"))     install.packages("lmtest")
if (!require("sandwich"))   install.packages("sandwich")
if (!require("dplyr"))      install.packages("dplyr")

library(AER)        # waldtest(), overid()
library(ivreg)      # ivreg()
library(lmtest)     # coeftest(), waldtest()
library(sandwich)   # vcovHC()
library(dplyr)
library(ggplot2)
library(car)
library(POE5Rdata)  # 資料內含 mroz 資料集

## 載入資料
data("mroz")

## 檢查資料結構
#str(data)
#summary(data)
#head(morz)
#tail(data)
#nrow(data)

## Data Prepare
mroz_lfp <- mroz |>                       # 428 位勞動參與者
  filter(lfp == 1) |>
  mutate(
    MOTHERCOLL = as.integer(mothereduc > 12),
    FATHERCOLL = as.integer(fathereduc > 12),
    exper2     = exper^2,
    lwage      = log(wage)
  )

## (a) 父母部份大學教育比率 --------------------------------------------
cat("\n=== (a) Percentage of Parents w/ College Education ===\n")
mroz_lfp |>
  summarise(MotherColl = round(mean(MOTHERCOLL)*100,2),
            FatherColl = round(mean(FATHERCOLL)*100,2)) |>
  print()

## (b) 相關係數 ---------------------------------------------------------
cat("\n=== (b) Correlations among EDUC, MOTHERCOLL, FATHERCOLL ===\n")
round(cor(with(mroz_lfp, cbind(educ, MOTHERCOLL, FATHERCOLL))), 3) |> print()

## (c)(d)  單一工具：MOTHERCOLL ----------------------------------------
cat("\n=== (c)(d) IV w/ Single Instrument (MOTHERCOLL) ===\n")
iv1 <- ivreg(
  lwage ~ educ + exper + exper2 |
    MOTHERCOLL + exper + exper2,
  data = mroz_lfp
)
print(summary(iv1, diagnostics = TRUE))

# robust 95% CI for β_EDUC
b1  <- coef(iv1)["educ"]
se1 <- sqrt(vcovHC(iv1, "HC1")["educ","educ"])
cat("95% CI for β_EDUC =", round(b1 + c(-1.96,1.96)*se1,3), "\n")

# 第一階段 robust F
fs1_full  <- lm(educ ~ MOTHERCOLL + exper + exper2, data = mroz_lfp)
fs1_restr <- update(fs1_full, . ~ . - MOTHERCOLL)
F1 <- waldtest(fs1_full, fs1_restr,
               vcov = vcovHC(fs1_full, "HC1"))$F[2]
cat("First-stage robust F (MOTHERCOLL) =", round(F1,2), "\n")

## (e)(f)  兩工具：MOTHERCOLL + FATHERCOLL -----------------------------
cat("\n=== (e)(f) IV w/ Two Instruments (MOTHERCOLL, FATHERCOLL) ===\n")
iv2 <- ivreg(
  lwage ~ educ + exper + exper2 |
    MOTHERCOLL + FATHERCOLL + exper + exper2,
  data = mroz_lfp
)
print(summary(iv2, diagnostics = TRUE))

# robust 95% CI for β_EDUC
b2  <- coef(iv2)["educ"]
se2 <- sqrt(vcovHC(iv2, "HC1")["educ","educ"])
cat("95% CI for β_EDUC =", round(b2 + c(-1.96,1.96)*se2,3), "\n")

# 第一階段聯合 robust F
fs2_full  <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + exper2, data = mroz_lfp)
fs2_restr <- update(fs2_full, . ~ . - MOTHERCOLL - FATHERCOLL)
F2 <- waldtest(fs2_full, fs2_restr,
               vcov = vcovHC(fs2_full, "HC1"))$F[2]
cat("First-stage robust F (two IVs) =", round(F2,2), "\n")

## (g) 過度識別檢定 -----------------------------------------------------
cat("\n=== (g) Over-identification Test (Hansen J) ===\n")

if ("overid" %in% ls("package:AER")) {
  print(AER::overid(iv2))
} else if ("sargan" %in% ls("package:ivreg")) {
  print(ivreg::sargan(iv2))
} else {
  # manual Hansen J
  u <- resid(iv2)
  Z <- model.matrix(~ MOTHERCOLL + FATHERCOLL + exper + exper2, data = mroz_lfp)
  J  <- nrow(Z) * summary(lm(u ~ Z - 1))$r.squared
  df <- ncol(Z) - 1                    # 2 instruments - 1 endogenous
  p  <- 1 - pchisq(J, df)
  cat("Hansen J =", round(J,3), "df =", df, "p-value =", round(p,3), "\n")
}

##############################################################################
#  螢幕輸出包含：
#  • (a) 百分比   • (b) 相關矩陣
#  • (c)(e) 2SLS 與 β_EDUC 95% CI (HC1-robust)
#  • (d)(f) 第一階段 HC1-robust F
#  • (g) Hansen J / Sargan 過度識別檢定
##############################################################################

