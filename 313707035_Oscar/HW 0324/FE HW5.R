# 載入資料與套件
library(POE5Rdata)
data("cocaine")

# 執行線性回歸
model <- lm(price ~ quant + qual + trend, data = cocaine)

# (b)顯示摘要結果
summary(model)

# (c)
summary(model)$r.squared

# (d)單邊檢定的 p-value
summary(model)$coefficients["quant", "Pr(>|t|)"] / 2

# (e)雙尾檢定的 p-value
summary(model)$coefficients["qual", "Pr(>|t|)"]

# (f)
summary(model)$coefficients["trend", "Estimate"]
