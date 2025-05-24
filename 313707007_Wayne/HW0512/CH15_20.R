# 載入資料
star <- read.csv("star.csv")

# 套用線性迴歸模型
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

# 檢視結果
summary(model_a)






# 載入必要套件
library(plm)

# 設定為 panel data frame（個體為學校，時間為學生 ID）
pdata <- pdata.frame(star, index = c("schid", "id"))

# 固定效果模型
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = pdata, model = "within", effect = "individual")

# 顯示結果
summary(model_b)





# pooled OLS 模型（無固定效果）
model_pooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pdata, model = "pooling")

# F 檢定（比較固定效果模型與 pooled 模型）
pFtest(model_b, model_pooled)
