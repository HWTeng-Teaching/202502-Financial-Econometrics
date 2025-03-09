#17

library(ggplot2)  
library(POE5Rdata)
data ("collegetown")
str(collegetown)



#17 
#a
# 繪製點陣圖與回歸線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", size = 2) +  # 設定點的顏色與大小
  labs(title = "price-sqft", x="Total square feet,100s", y="Sale price,1000$")






#b 得出回歸線與圖

run_line <- function(price, sqft) {
  model <- lm(price ~ sqft)  # 執行線性回歸
  return(summary(model))
  
}


model <- run_line(collegetown$price,collegetown$sqft)
print(model)


# 繪製點陣圖與回歸線（線性）
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", size = 2) +  # 設定點的顏色與大小
  labs(title = "price-sqft", x = "Total square feet,100s", y = "Sale price,1000$") +
  geom_smooth(method = "lm", se = FALSE, color = "red")





#c 非線性函數, 還有彈性，當面積在20時，每增加一單位，價格會增加多少

mod_square <- lm(price~I(sqft^2), data=collegetown)
b1 <- coef(mod_square)[[1]]
b2 <- coef(mod_square)[[2]]
summary(mod_square)

sqftx = c(20) #given values for sqft
pricex = b1+b2*sqftx^2 #prices corresponding to given sqft 

DpriceDsqft <- 2*b2*sqftx # 斜率

elasticity=DpriceDsqft*sqftx/pricex 
b1; b2; DpriceDsqft; elasticity #prints results




#d
# 繪製點陣圖與線（非線性）

x0 <- 20
y0 <- pricex

mod_square <- lm(price~I(sqft^2), data=collegetown)

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "grey", size = 2) +  # 設定點的顏色與大小
  labs(title = "price-sqft", x="Total square feet, 100s", y="Sale price,1000$") +
  stat_function(fun = function(x) b1 + b2 * x^2, color = "red")+
  geom_abline(slope = DpriceDsqft, intercept = y0 - DpriceDsqft * x0,color = 'green')+
  annotate("point", x = x0, y = y0, color = "blue", size = 3)

 


#e 計算彈性

elasticity=DpriceDsqft*sqftx/pricex 

print(elasticity)


#f 計算殘差

collegetown$residuals_linear <- residuals(model)
collegetown$residuals_quadratic <- residuals(mod_square)


ggplot(collegetown, aes(x = sqft, y = residuals_linear )) +
  geom_point(color = "blue", size = 2) +  # 設定點的顏色與大小
  labs(title = "sqft- residuals_linear", x="Total square feet,100s", y="residuals_linear")

ggplot(collegetown, aes(x = sqft, y = residuals_quadratic)) +
  geom_point(color = "blue", size = 2) +  # 設定點的顏色與大小
  labs(title = "sqft- residuals_quadraticr", x="Total square feet,100s", y="residuals_quadratic")


#g 最小SSE


sse_linear <- sum(residuals(model)^2)
sse_quadratic <- sum(residuals(mod_square)^2)


sse_linear
sse_quadratic





