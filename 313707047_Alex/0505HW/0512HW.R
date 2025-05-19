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


