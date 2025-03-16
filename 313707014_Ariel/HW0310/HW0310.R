library(dplyr)
library(ggplot2)
library(lmtest)

data("motel")
summary(motel)
head(motel)


#a.

ggplot(data = motel, aes(x = time)) +

  geom_line(aes(y = motel_pct, color = "Motel_PCT") )+
  geom_line(aes(y = comp_pct, color = "Comp_PCT")) +
  labs(x = "Time", y = "Occupancy Rate (%)", color = "Series") +
  ggtitle("Occupancy Rates Over Time") +
  theme_minimal()



#找出回歸線，95% 信心水準 interval 


mod <- lm(motel_pct ~ comp_pct, data=motel) #創建回歸
smod <- summary(mod)
smod

alpha1 <- 0.05

b1 <- coef(mod)[[1]]
b2 <- coef(mod)[[2]]
b1
b2

df1 <- df.residual(mod)  #計算自由度
df1

seb2 <- coef(smod)[2,2]
seb2

tc <- qt(1-alpha1/2, df1)
lowb <- b2-tc*seb2 
upb <- b2+tc*seb2   
lowb  #下界
upb   #上界

cat( "[", lowb ,upb, "]")


#b.取得 90% 信賴區間 (對平均值)
# 競爭者入住率 = 70， 為了符合 predict, 所以要用 dataframe 的格式
newdata <- data.frame(comp_pct = 70)

predict(mod, newdata, interval = "confidence", level = 0.90)


#c. 假設檢定 H0=0
alpha_99 <- 0.01
t_value <- b2/seb2
critical_value <- qt(1-alpha_99, df1)
t_value 
critical_value #在99％信心水準下的t值

if (t_value > critical_value) {
  cat("結論：拒絕 H0。\n")
} else {
  cat("結論：無法拒絕 H0。\n")
}





#d. H0 = 1
c <- 1

t_value_d <- (b2-c)/seb2
t_value_d

critical_value_d <- qt(1-alpha_99/2, df1) 
critical_value_d


if (t_value_d > critical_value_d) {
  cat("結論：拒絕 H0。\n")
} else {
  cat("結論：無法拒絕 H0。\n")
}


# e. 畫出殘差圖並且解釋 x=17 和 x=23 時的特點

residual <- residuals(mod)
residual

residual_data <- data.frame(time = motel$time, residual = residual)

ggplot(data = residual_data, aes(x = time)) +
  
  geom_line(aes(y = residual), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +  # 加 y=0 水平線
  geom_vline(xintercept = c(17, 23), , color = "black") +  # 加 x=17 和 x=23 垂直線
  labs(x = "Time", y = "residual", color = "Series") +
  ggtitle("residual Over Time") +
  theme_minimal()







