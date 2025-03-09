
library(ggplot2)  
library(POE5Rdata)
library(dplyr)
library(psych)

data ("cex5_small")
head(cex5_small)

#a. 
hist(cex5_small$foodaway,breaks =50, col='grey')



describe(cex5_small$foodaway)
summary(cex5_small$foodaway)



#b.
#advance degree

advanced_data <- cex5_small %>%
  select(advanced,foodaway)

filtered_data_ad <- advanced_data %>%
  filter(advanced == 1)  
describe(filtered_data_ad$foodaway)


#college degree

college_data <- cex5_small %>%
  select(college,foodaway)

filtered_data_co <- college_data %>%
  filter(college == 1)  
describe(filtered_data_co$foodaway)


#none

none_data <- cex5_small %>%
  select(advanced,college,foodaway)


filtered_data_no <- none_data %>%
  filter(college == 0 & advanced == 0)  
describe(filtered_data_no$foodaway)


#c. 針對foodaway取log
cex5_clean <- cex5_small %>%
  filter(foodaway > 0)  # 避免 log(0) 變成 -Inf

log_data <- log(cex5_clean$foodaway)
log_data
describe(log_data)

hist(log_data,breaks =50, col='grey')
describe(cex5_clean)


#d. 跑回歸

income_data <- cex5_clean$income
log_data <- data.frame(log_data)
income_data <- data.frame(income_data)

comb_data <- cbind(log_data,income_data) #創建資料集
head(comb_data)



mod <- lm(log_data ~ income_data, data=comb_data) #創建回歸
summary(mod)
b1 <- coef(mod)[[1]]
b2 <- coef(mod)[[2]]

#e 畫點陣圖與回歸線
ggplot(comb_data, aes(x = income_data, y = log_data))+
  geom_point(color = "grey", size = 2)+
  labs (title = "income_food", x = "income", y = "ln(foodaway)")+
  stat_function(fun = function(x) b1 + b2*x, color = "red" )



#f 殘差
residual <- residuals(mod)
residual <- data.frame(residual)

residual_income <- cbind(residual,income_data)

ggplot(residual_income, aes(x = income_data, y =residual ))+
  geom_point(color = "grey", size = 2)+
  labs (title = "income_food", x = "income", y = "residuals")
  
  






