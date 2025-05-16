# 一次性安裝資料套件
install.packages("remotes")               # 安裝 remotes
remotes::install_github("ccolonescu/POE5Rdata")  # 下載 POE5Rdata 套件

# 一次性修正程式碼
library(POE5Rdata)
library(plm)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(dplyr)

# 讀取資料
data("star")

# 檢查欄位名稱確認（選擇性）
colnames(star)

# 正確設定 panel 資料格式
pdata <- pdata.frame(star, index = c("schid", "id"))

# (a) OLS regression
ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(ols_model)

# (b) 固定效果模型（學校固定效果）
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within", effect = "individual")
summary(fe_model)

# (c) F test 固定效果是否顯著
pFtest(fe_model, ols_model)

# (d) 隨機效果模型與 LM test
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "random")
summary(re_model)

plmtest(re_model, type = "bp")

# (e) Hausman test
phtest(fe_model, re_model)

# Robust 標準誤比較
fe_robust <- coeftest(fe_model, vcov = vcovCR(fe_model, type = "CR2"))
re_robust <- coeftest(re_model, vcov = vcovHC(re_model, type = "HC1"))

fe_robust
re_robust

# (f) Mundlak test

# 使用 between 模型直接檢驗 group mean 變數
mundlak_between <- plm(
  readscore ~ small_mean + aide_mean + tchexper_mean + boy_mean + white_asian_mean + freelunch_mean,
  data = pdata_mundlak,
  model = "between"
)

summary(mundlak_between)


