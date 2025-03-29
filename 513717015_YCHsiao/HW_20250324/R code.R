#CH5Q03 c

# Compute 95% confidence interval for beta_4
b4 <- -0.1503
se_b4 <- 0.0235
z_critical <- 1.96  # for 95% CI

lower_95 <- b4 - z_critical * se_b4
upper_95 <- b4 + z_critical * se_b4

c(lower_95, upper_95)

#CH5Q03 d

b3_hat <- -1.4549
se_b3 <- 0.3695
b3_null <- -2

t_value <- (b3_hat - b3_null) / se_b3
t_value


#Ch5Q23

#a.
# 安裝資料集（若尚未安裝）
# install.packages("remotes")
# remotes::install_github("ccolonescu/POE5Rdata")

library(POE5Rdata)
data("cocaine")

# 查看資料
head(cocaine)

# 執行迴歸分析
model <- lm(price ~ quant + qual + trend, data = cocaine)

# 查看回歸摘要
summary(model)


#b.

