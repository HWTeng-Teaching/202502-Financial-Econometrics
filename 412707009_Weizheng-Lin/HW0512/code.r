# 載入所需資料
library(POE5Rdata)

# 第15.17題 

# 載入 liquor5 資料
data("liquor5")

# 按照家庭編號與年份排序，以便正確計算差分
liquir <- liquor5[order(liquor5$hh, liquor5$year), ]

# 為每個家庭計算 liquor 和 income 的一次差分
liquir$LIQUORD <- ave(liquir$liquor, liquir$hh, FUN = function(x) c(NA, diff(x)))
liquir$INCOMED <- ave(liquir$income, liquir$hh, FUN = function(x) c(NA, diff(x)))

# 移除差分後產生的 NA 值（每個家庭的第一筆觀察）
liquir <- liquir[!is.na(liquir$LIQUORD), ]

# 對 LIQUORD 與 INCOMED 進行 OLS 回歸
model <- lm(LIQUORD ~ INCOMED, data = liquir)

# 顯示回歸摘要
summary(model)

# 計算 INCOMED 係數的 95% 信賴區間
confint(model, "INCOMED", level = 0.95)


# 第15.20題 

# 載入 star 資料
data("star")

# (a) 不含固定效應的回歸模型
model_a <- lm(readscore ~ small + aide + tchexper + white_asian + freelunch, data = star)
summary(model_a)

# (b) 加入學校固定效應
model_b <- lm(readscore ~ small + aide + tchexper + white_asian + freelunch + factor(schid), data = star)
summary(model_b)

# 比較兩個模型的迴歸係數
summary(model_a)$coefficients
summary(model_b)$coefficients

# (c) 使用 ANOVA 比較模型，檢驗學校固定效應是否顯著
anova(model_a, model_b)
