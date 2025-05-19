#15.17
library(POE5Rdata)
data("liquor5")

#(a)
# 按照家庭與年份排序
liquor5 <- liquor5[order(liquor5$hh, liquor5$year), ]

# 建立第一差分
liquor5$liquord <- ave(liquor5$liquor, liquor5$hh, FUN = function(x) c(NA, diff(x)))
liquor5$incomed <- ave(liquor5$income, liquor5$hh, FUN = function(x) c(NA, diff(x)))

# 拿掉 NA
df <- na.omit(liquor5[, c("liquord", "incomed")])

# 無截距回歸
model <- lm(liquord ~ incomed + 0, data = df)

# 顯示結果與 95% 信賴區間
summary(model)
confint(model)

#15.20
library(POE5Rdata)
data("star")

#(a)
model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model)

#(b)
# 改掉 id 這個變數名稱
star$stid <- star$id
star$id <- NULL  # 刪除原本的 id 欄位（避免混淆）

# 指定學校為個體（做學校 fixed effects）
pdata <- pdata.frame(star, index = c("schid", "stid"))

# 跑學校固定效果模型
fe_school <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata,
                 model = "within",
                 effect = "individual")
summary(fe_school)

#(c)
# 無固定效果的 OLS 模型
ols_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata,
                 model = "pooling")

# 固定效果模型（學校固定效果）
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata,
                model = "within",
                effect = "individual")

# F 檢定
pFtest(fe_model, ols_model)

