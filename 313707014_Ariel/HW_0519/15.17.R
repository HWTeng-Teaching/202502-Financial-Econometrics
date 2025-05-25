#15.17


library(POE5Rdata)

library(dplyr)
library(lmtest)
library(sandwich)
library(broom)
library(plm)

data ("liquor5")
summary(liquor5)
head(liquor5)

#a. 差分法，控制固定效果
liquor <- liquor5 %>%
  arrange(hh, year)

# 建立 first-differenced 變數：就是t = t+1 - t，在做差分
liquor <- liquor %>%
  group_by(hh) %>%
  mutate(
    liquord = lead(liquor) - liquor,
    incomed = lead(income) - income
  ) %>%
  ungroup()

#排除NA
liquor_diff <- liquor %>%
  filter(!is.na(liquord), !is.na(incomed))

# 無截距項回歸（OLS）
model <- lm(liquord ~ incomed - 1, data = liquor_diff)

# 顯示回歸摘要
summary(model)

# 使用 heteroskedasticity-robust 標準誤建立 95% 信賴區間

# 計算 robust 標準誤
robust_vcov <- vcovHC(model, type = "HC1")
robust_se <- sqrt(diag(robust_vcov))  # robust 標準誤
estimate <- coef(model)[1]            # 回歸係數
df <- model$df.residual               # 自由度
t_crit <- qt(0.975, df)               # 臨界值

# 手動計算信賴區間
lower <- estimate - t_crit * robust_se
upper <- estimate + t_crit * robust_se

cat("Robust 95% 信賴區間：(", round(lower, 4), ",", round(upper, 4), ")\n")


#b
model_b <- plm(liquor ~ income, data = liquor, model = "random")
summary(model_b)

#c
plmtest(liquor ~ income, data = liquor, type = "bp")



#d
# 建立 INCOMEM（每個人的收入平均）
liquor$INCOMEM <- ave(liquor$income, liquor$hh)

# 使用 RE 模型並加上 INCOMEM（Mundlak 方法）
model_d <- plm(liquor ~ income + INCOMEM, data = liquor, model = "random")
summary(model_d)

# 檢定 γ 的顯著性
coeftest(model_d, vcovHC(model_d, type = "HC1"))





