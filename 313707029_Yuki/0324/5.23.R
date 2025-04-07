# 清除環境變數（可選）
rm(list=ls())

# 安裝並載入所需套件
library(POE5Rdata)
library(dplyr)
library(ggplot2)

# 載入資料
data("cocaine")

model <- lm(PRICE ~ QUANT + QUAL + TREND, data = cocaine)
summary(model)
