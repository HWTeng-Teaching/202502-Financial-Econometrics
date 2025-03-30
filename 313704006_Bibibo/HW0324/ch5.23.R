library(POE5Rdata)
data("cocaine")

#b, c
lr = lm(price ~ quant+qual+trend, data=cocaine)
sum_lr = summary(lr)

#d
#H0:Beta2>=0, Ha:Beta<0
estBeta2 = coef(sum_lr)[2, 1]
seBeta2 = coef(sum_lr)[2, 2]
test_statistic = (estBeta2-0)/seBeta2
df = df.residual(lr)
tc = qt(0.05, df, lower.tail = FALSE)
abs(test_statistic) > tc #TRUE: reject H0

#e
#H0:Beta3=0, Ha:Beta3>=0
estBeta3 = coef(sum_lr)[3, 1]
seBeta3 = coef(sum_lr)[3, 2]
test_statistic = (estBeta3-0)/seBeta3
df = df.residual(lr)
tc = qt(0.025, df, lower.tail = FALSE)
abs(test_statistic) > tc #FALSE: non-reject H0

#f
price = rep(0, 8) # 長度 8 的向量
count = rep(0, 8)
for (i in 1:nrow(cocaine)) {
  index =  cocaine[i, 4]
  price[index] = price[index] + cocaine[i, 1]
  count[index] = count[index] + 1
}

average_annual_price = price/count
annual_pc = rep(0, 7)
for (i in 1:7){
  annual_pc[i] = (average_annual_price[i+1]-average_annual_price[i]) / average_annual_price[i]
}

# 只計算非 NA 值的平均
average_annual_pc_geom = prod(1 + annual_pc, na.rm = TRUE)^(1/4) - 1
