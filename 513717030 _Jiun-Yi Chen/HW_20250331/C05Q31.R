install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata")

library(POE5Rdata)
data(commute5)
data <- commute5

str(data)  # Check data
summary(data)  

model <- lm(time ~ depart + reds + trains, data = data)

# 顯示模型摘要結果
summary(model)

# 找出共變異數
model <- lm(time ~ depart + reds + trains, data = data)
vcov_matrix <- vcov(model)
print(vcov_matrix)

cov_reds_trains <- vcov_matrix["reds", "trains"]
print(cov_reds_trains)
