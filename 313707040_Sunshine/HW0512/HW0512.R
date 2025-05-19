#CH15Q17
#(a)
# 1. 載入套件與資料
install.packages("wooldridge")
library(wooldridge)   # 包含 liquor 資料
library(dplyr)
library(POE5Rdata)
data("liquor5")        # 40 戶，3 年觀察
# liquor 變數：id、year、LIQUOR（千美元）、INCOME（千美元）

# 2. 依 id, year 排序、做一階差分
df_diff <- liquor5 %>% 
  arrange(hh, year) %>% 
  group_by(hh) %>% 
  mutate(
    LIQUORD  = liquor  - lag(liquor),
    INCOMED  = income  - lag(income)
  ) %>% 
ungroup() %>% 
filter(!is.na(LIQUORD))   # 去掉每戶第一年的 NA

# 3. 用 OLS（不含常數項）回歸 LIQUORD 對 INCOMED
mod <- lm(LIQUORD ~ 0 + INCOMED, data = df_diff)

# 4. 檢視估計結果
summary(mod)

# 5. 95% 信賴區間
confint(mod, level = 0.95)


#CH15Q20
#(a)
# 1. 載入套件和資料
library(plm)          # 面板迴歸
data("star")          
df <- star
# 直接 OLS 迴歸（無任何 fixed/random effects）
ols1 <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
           data = df)
summary(ols1)

# (b) 加入「學校固定效果」的 within‐school FE 模型
pdf <- pdata.frame(df, index = c("schid","id"))
fe_sch <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = pdf, model = "within")
summary(fe_sch)

# (c) 檢定學校固定效果是否顯著
pool1 <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
             data = pdf, model = "pooling")
pFtest(fe_sch, pool1)

