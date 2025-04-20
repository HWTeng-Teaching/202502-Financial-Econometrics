#a
# 讀取資料
commute5 <- read.csv("commute5.csv")

# 查看變數名稱
names(commute5)

# 執行線性迴歸
model <- lm(time ~ depart + reds + trains, data = commute5)

# 顯示摘要結果
summary(model)

#b
confint(model, level = 0.95)

vcov(model)
