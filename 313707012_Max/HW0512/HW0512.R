library(POE5Rdata)
data("liquor5")

#15.17
#a.------------------------------------------------------
# Sort data by hh (household) and year to ensure proper differencing
liquir <- liquor5[order(liquor5$hh, liquor5$year), ]

# Create first-differenced variables LIQUORD and INCOMED
liquir$LIQUORD <- ave(liquir$liquor, liquir$hh, 
                      FUN = function(x) c(NA, diff(x)))
liquir$INCOMED <- ave(liquir$income, liquir$hh, 
                      FUN = function(x) c(NA, diff(x)))

# Remove NA rows (first year for each household after differencing)
liquir <- liquir[!is.na(liquir$LIQUORD), ]

# Run OLS regression of LIQUORD on INCOMED with a constant
model <- lm(LIQUORD ~ INCOMED, data = liquir)

# Display regression summary
summary(model)

# Compute 95% confidence interval for the coefficient of INCOMED
conf_interval <- confint(model, "INCOMED", level = 0.95)
conf_interval


#15.20
#a.------------------------------------------------------
# 載入資料
library(POE5Rdata)
data("star")

model_a <- lm(readscore ~ small + aide + tchexper + white_asian + freelunch, data = star)

# 顯示回歸結果
summary(model_a)

#b.------------------------------------------------------
# 加入學校固定效應
model_b <- lm(readscore ~ small + aide + tchexper + white_asian + freelunch + factor(schid), data = star)

# 顯示回歸結果
summary(model_b)

# 比較兩個模型的係數
summary(model_a)$coefficients
summary(model_b)$coefficients

#c.------------------------------------------------------
# 使用 anova 比較模型，檢驗固定效應的顯著性
anova(model_a, model_b)

