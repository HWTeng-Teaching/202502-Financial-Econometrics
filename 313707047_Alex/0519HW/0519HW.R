##CH15.17
##A
rm(list=ls()) 
library(POE5Rdata)
data(liquor5)


# 2. 輔助：檢查結構
head(liquor5)
# 確保變數名稱是 LIQUOR 和 INCOME，還有 household ID 與 year

# 3. 排序後做一階差分

library(dplyr)

df <- liquor5 %>%
  arrange(hh, year) %>%         # ✅ 排序
  group_by(hh) %>%              # ✅ 每戶家庭做差分
  mutate(
    LIQUORD = liquor - lag(liquor),     # ✅ 一階差分
    INCOMED = income - lag(income)
  ) %>%
  ungroup()

# 移除 NA（第一年差分會是 NA）
df_clean <- df %>% filter(!is.na(LIQUORD) & !is.na(INCOMED))

# 回歸（無截距項）
model <- lm(LIQUORD ~ INCOMED - 1, data = df_clean)
summary(model)

# 95% 信賴區間
confint(model, level = 0.95)



##B
library(plm)
# 清空目前所有變數（尤其可能是函數命名衝突）
rm(list = ls())

# 重新載入套件
library(plm)

# 轉換成 panel 資料格式
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

# Random effects 模型
re_model <- plm(liquor ~ income, data = pdata, model = "random")

summary(re_model)

# 提取估計值與標準誤差，建構 95% 信賴區間
coefs <- summary(re_model)$coefficients
beta2 <- coefs["income", "Estimate"]
se_beta2 <- coefs["income", "Std. Error"]

# 信賴區間
lower <- beta2 - 1.96 * se_beta2
upper <- beta2 + 1.96 * se_beta2

cat("95% CI for income coefficient: [", round(lower, 4), ",", round(upper, 4), "]\n")


##C
# Breusch-Pagan LM test for random effects
plmtest(liquor ~ income, data = pdata, type = "bp")


##D
# 計算個體的 income 平均（INCOMEM）
pdata$incomem <- ave(pdata$income, pdata$hh, FUN = mean)

# Random effects 模型中加入 incomem
re_model_extended <- plm(liquor ~ income + incomem, data = pdata, model = "random")

summary(re_model_extended)


##CH15.20
rm(list=ls()) 
library(POE5Rdata)
data("star")     # 確保你有載入正確的資料集

##A
# 2. 檢查資料結構（可選）
head(star)
str(star)

# 3. OLS 回歸（不含固定/隨機效果）
model_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

# 4. 查看結果
summary(model_ols)

# 5. 95% 信賴區間（可選）
confint(model_ols)


##B
# 1. 載入套件
install.packages("plm")

library(plm)
pdata <- pdata.frame(star, index = "schid")
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within")

summary(fe_model)



##c
# 檢定：學校固定效果是否顯著
ols_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata, model = "pooling")

pFtest(fe_model, ols_model)



##D

library(plm)

# 轉換成 panel data 格式（假設資料已包含 school ID 和年）
pdata <- pdata.frame(star, index = c("schid", "id"))

# 隨機效果模型
re_model <- plm(readscore ~ small + aide + tchexper + white_asian + freelunch + boy, 
                data = pdata, model = "random")

summary(re_model)

# LM 檢定是否應使用隨機效果（vs pooled OLS）
plmtest(readscore ~ small + aide + tchexper + white_asian + freelunch + boy, 
        data = pdata, type = "bp")



##E
library(plm)

# FE 模型
fe_model <- plm(readscore ~ small + aide + tchexper + white_asian + freelunch + boy, 
                data = pdata, model = "within")

# RE 模型
re_model <- plm(readscore ~ small + aide + tchexper + white_asian + freelunch + boy, 
                data = pdata, model = "random")

# 自訂函數計算 t 值
compare_fe_re <- function(varname) {
  b_fe <- coef(summary(fe_model))[varname, "Estimate"]
  se_fe <- coef(summary(fe_model))[varname, "Std. Error"]
  
  b_re <- coef(summary(re_model))[varname, "Estimate"]
  se_re <- coef(summary(re_model))[varname, "Std. Error"]
  
  t_val <- (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)
  
  return(round(t_val, 3))
}


# 自訂函數計算 t 值
compare_fe_re <- function(varname) {
  b_fe <- coef(summary(fe_model))[varname, "Estimate"]
  se_fe <- coef(summary(fe_model))[varname, "Std. Error"]
  
  b_re <- coef(summary(re_model))[varname, "Estimate"]
  se_re <- coef(summary(re_model))[varname, "Std. Error"]
  
  t_val <- (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)
  
  return(round(t_val, 3))
}

vars <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

t_stats <- sapply(vars, compare_fe_re)
names(t_stats) <- vars

print(t_stats)



fe_model <- plm(readscore ~ small + aide + tchexper + white_asian + freelunch + boy, 
                data = pdata, model = "within")
summary(fe_model)


phtest(fe_model, re_model)


##F
# 對每個學校計算變數平均值，命名為 *_m
pdata$small_m       <- ave(pdata$small,       pdata$schid)
pdata$aide_m        <- ave(pdata$aide,        pdata$schid)
pdata$tchexper_m    <- ave(pdata$tchexper,    pdata$schid)
pdata$boy_m         <- ave(pdata$boy,         pdata$schid)
pdata$white_asian_m <- ave(pdata$white_asian, pdata$schid)
pdata$freelunch_m   <- ave(pdata$freelunch,   pdata$schid)

# 確保無 NA 值
pdata_clean <- na.omit(pdata)

# Mundlak 模型
mundlak_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                       small_m + aide_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
                     data = pdata_clean,
                     model = "random")

summary(mundlak_model)