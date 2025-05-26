if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("car")  
library(car)
install.packages("AER")  
library(AER)
library(POE5Rdata)
data('liquor5')
library(dplyr)

liquor_diff <- liquor5 %>%
  arrange(hh, year) %>%
  group_by(hh) %>%
  mutate(
    liquord = liquor - lag(liquor),
    incomed = income - lag(income)
  ) %>%
  ungroup() %>%
  filter(!is.na(liquord) & !is.na(incomed))  # 去除 NA 差分

# 執行無截距的線性回歸 liquord ~ incomed - 1
model <- lm(liquord ~ incomed - 1, data = liquor_diff)

# 顯示回歸結果
summary(model)

# 建立 95% 信賴區間
confint(model, level = 0.95)

# (b)  隨機效果之Inocme信賴區間
library(plm)
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

# 隨機效果模型估計
re_model <- plm(liquor ~ income, data = pdata, model = "random")

# 顯示模型摘要
summary(re_model)

# 建立 95% 信賴區間
confint(re_model)

# (c) 使用 LM 檢定檢查是否存在隨機效果
plmtest(liquor ~ income, data = pdata, effect = "individual", type = "bp")

# (d) 加入個體平均
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(incomem = mean(income)) %>%
  ungroup()

# 更新 panel 資料格式
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

# 估計含個體平均 INCOME 的模型（即 Hausman-Taylor）
re_with_avg_model <- plm(liquor ~ income + incomem, data = pdata, model = "random")

# 顯示模型摘要（檢查 incomem 的係數是否顯著）
summary(re_with_avg_model)
