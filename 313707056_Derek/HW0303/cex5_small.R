library(tidyverse)
library(POE5Rdata)
library(dplyr)
data("cex5_small")
?cex5_small
?geom_histogram
ggplot(data=cex5_small,aes(foodaway))+
  geom_histogram(binwidth = 10, fill = "blue", color = "black")
mean_foodaway <- mean(cex5_small$foodaway)
median_foodaway <- median(cex5_small$foodaway)
quantiles_foodaway <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75))

advance_degree<-cex5_small %>% filter(advanced==1)
mean(advance_degree$foodaway)
median(advance_degree$foodaway)

master_degree<-cex5_small %>% filter(college==1)
mean(master_degree$foodaway)
median(master_degree$foodaway)

No_master_degree<-cex5_small %>% filter(college==0,advanced==0)
mean(No_master_degree$foodaway)
median(No_master_degree$foodaway)


log_foodaway<-cex5_small %>%
  mutate(foodaway=ifelse(foodaway> 0, log(foodaway), NA))
  
ggplot(data=log_foodaway,aes(foodaway))+
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black")+
  labs(x="ln(foodaway)")
summary(log_foodaway)


m1<-lm(foodaway~income,data = log_foodaway)
summary(m1)
ggplot(data=log_foodaway,aes(x=income,y=foodaway))+
  geom_point(color="blue")+
  geom_abline(slope = m1$coefficients[2],intercept = m1$coefficients[1],
              color="red")


SSE<-sum(m1$residuals^2)
residuals<-m1$residuals
new_log_foodaway<-log_foodaway%>%
  drop_na(foodaway)
ggplot(new_log_foodaway, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs INCOME", x = "INCOME ($100 units)", y = "Residuals")

runs.test(m1$residuals)
shapiro.test(m1$residuals)
ncvTest(m1)

