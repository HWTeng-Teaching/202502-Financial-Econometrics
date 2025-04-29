############################################################
# Econometrics 10.24  ── IV/2SLS 與各種標準誤：R 範例程式
############################################################

## 1. 套件 --------------------------------------------------
# 若第一次使用請先安裝：install.packages(c("AER","sandwich","lmtest","boot","ggplot2","readr","dplyr"))
library(readr)      # 讀取 csv
library(dplyr)      # 資料處理
library(AER)        # ivreg()
library(sandwich)   # vcovHC()：White/HC1 穩健共變異矩陣
library(lmtest)     # coeftest()：配合自訂共變異矩陣輸出
library(boot)       # bootstrap
library(ggplot2)    # 繪圖

## 2. 讀檔並前處理 ------------------------------------------
mroz <- read_csv("mroz.csv")

mroz_lf <- mroz %>%                                # 僅保留參與勞動市場者
  filter(lfp == 1, wage > 0) %>%                   # 預防 log(0)
  mutate(
    lwage  = log(wage),
    exper2 = exper^2
  )

## 3. 基準 IV / 2SLS ---------------------------------------
iv_base <- ivreg(
  lwage ~ educ + exper + exper2 |                  # 第二階段方程
    mothereduc + fathereduc + exper + exper2,      # 所有工具 + 外生變數
  data = mroz_lf
)

summary(iv_base)                                   # 傳統標準誤

## 4. (a) 取殘差並作圖 --------------------------------------
mroz_lf <- mroz_lf %>% mutate(resid_iv = resid(iv_base))

ggplot(mroz_lf, aes(exper, resid_iv)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "loess", se = FALSE, linewidth = .7) +
  labs(x = "EXPER", y = "IV residuals", 
       title = "Residuals vs. Experience (2SLS)") +
  theme_minimal()

## 5. (b) Breusch–Pagan NR² 檢定 ----------------------------
bp_aux   <- lm(I(resid_iv^2) ~ exper, data = mroz_lf)
NR2      <- nobs(bp_aux) * summary(bp_aux)$r.squared
p_BP     <- 1 - pchisq(NR2, df = 1)                # χ²(1) p-value

## 6. (c) White / HC1 穩健標準誤 -----------------------------
rob_vcov <- vcovHC(iv_base, type = "HC1")
coeftest(iv_base, vcov = rob_vcov)                 # 列出 HC1 t 值

rob_se   <- sqrt(rob_vcov["educ","educ"])
ci_rob   <- coef(iv_base)["educ"] + c(-1, 1)*1.96*rob_se

## 7. (d) Bootstrap (B = 200) ------------------------------
set.seed(123)                                      # 可重現
boot_fun <- function(data, indices){
  d <- data[indices, ]
  coef(
    ivreg(lwage ~ educ + exper + exper2 | 
            mothereduc + fathereduc + exper + exper2,
          data = d)
  )["educ"]
}

boot_out <- boot(mroz_lf, statistic = boot_fun, R = 200)
boot_se  <- sd(boot_out$t)
ci_boot  <- coef(iv_base)["educ"] + c(-1, 1)*1.96*boot_se

## 8. 彙整輸出 ---------------------------------------------
results <- list(
  "Breusch–Pagan NR2"      = NR2,
  "BP p-value"             = p_BP,
  "Baseline SE (educ)"     = summary(iv_base)$coef["educ","Std. Error"],
  "Robust SE (HC1)"        = rob_se,
  "95% CI Robust"          = ci_rob,
  "Bootstrap SE (B=200)"   = boot_se,
  "95% CI Bootstrap"       = ci_boot
)

print(results)
