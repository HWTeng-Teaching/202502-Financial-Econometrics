# C10Q20 

##############################################################################
# 前置設定：清除環境、載入套件與資料
##############################################################################
rm(list = ls())  # 清除工作環境

### 載入必要套件
##套件與資料 ------------------------------------------------------------
if (!require("AER"))        install.packages("AER")
if (!require("tidyverse"))  install.packages("tidyverse")
if (!require("lmtest"))     install.packages("lmtest")
if (!require("sandwich"))   install.packages("sandwich")
if (!require("dplyr"))      install.packages("dplyr")

library(AER)        # ivreg(), waldtest(), overid()
library(tidyverse)  
library(lmtest)     # coeftest(), waldtest()
library(sandwich)   # vcovHC()
library(dplyr)
library(ggplot2)
library(car)
library(POE5Rdata)  # 資料內含 capm5 資料集

## 載入資料
data("capm5")

## 檢查資料結構
#str(data)
#summary(data)
#head(data)
#tail(data)
#nrow(data)

## 建立超額報酬與工具變數 --------------------------------------------
capm5 <- capm5 %>% 
  mutate(
    msft_rf = msft - riskfree,      # Microsoft 超額報酬
    rm_rf   = mkt  - riskfree,      # 市場超額報酬
    RANK    = rank(rm_rf, ties.method = "first"),
    POS     = if_else(rm_rf > 0, 1, 0)
  )

## (a) OLS 估計 -------------------------------------------------------
ols_fit <- lm(msft_rf ~ rm_rf, data = capm5)
summary(ols_fit)

## (b) 第一階段：rm_rf ~ RANK ----------------------------------------
first_rank <- lm(rm_rf ~ RANK, data = capm5)
summary(first_rank)
capm5 <- capm5 %>% mutate(vhat = resid(first_rank))

## (c) 外生性檢定（加入 vhat） ---------------------------------------
aug_fit <- lm(msft_rf ~ rm_rf + vhat, data = capm5)
coeftest(aug_fit)          # vhat 的顯著性
# 若需 1% 顯著水準比較：查看 p-value < 0.01？

##v(d) 2SLS：單一工具 RANK -------------------------------------------
iv1_fit <- ivreg(msft_rf ~ rm_rf | RANK, data = capm5)
summary(iv1_fit, diagnostics = TRUE)

## (e) 第一階段：rm_rf ~ RANK + POS ----------------------------------
first_rank_pos <- lm(rm_rf ~ RANK + POS, data = capm5)
summary(first_rank_pos)
capm5 <- capm5 %>% mutate(vhat2 = resid(first_rank_pos))

## (f) Hausman 檢定 ---------------------------------------------------
hausman_test <- lm(msft_rf ~ rm_rf + vhat2, data = capm5)
coeftest(hausman_test)     # 檢查 vhat2 在 1% 水準是否顯著

## (g) 2SLS：雙工具 RANK + POS ---------------------------------------
iv2_fit <- ivreg(msft_rf ~ rm_rf | RANK + POS, data = capm5)
summary(iv2_fit, diagnostics = TRUE)

## (h) Breusch–Pagan 檢定（2SLS 殘差） -------------------------------
capm5 <- capm5 %>% mutate(iv_resid = resid(iv2_fit))

bp_test <- bptest(iv_resid ~ fitted(iv2_fit), data = capm5)
bp_test    # 在 5% 顯著水準檢視 p-value

