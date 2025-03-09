library(tidyverse)
library(POE5Rdata)
library(dplyr)
data("cps5_small")
summary(cps5_small)
ggplot(data=cps5_small,aes(x=educ))+
  geom_histogram(binwidth=1,fill="blue",color="black")
ggplot(data=cps5_small,aes(x=wage))+
  geom_histogram(binwidth=1,fill="blue",color="black")


m1<-lm(wage~educ,data=cps5_small)
summary(m1)


SSE<-sum(m1$residuals^2)
residuals<-m1$residuals
plot(residuals)
ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs EDUC", x = "EDUC", y = "Residuals")
?cps5_small

female<-cps5_small %>%
  filter(female==1)
m1_female<-lm(wage~educ,data=female)
summary(m1_female)

male<-cps5_small %>%
  filter(female==0)
m1_male<-lm(wage~educ,data=male)
summary(m1_male)

black<-cps5_small %>%
  filter(black==1)
m1_black<-lm(wage~educ,data=black)
summary(m1_black)

white<-cps5_small %>%
  filter(black==0)
m1_white<-lm(wage~educ,data=white)
summary(m1_white)


m2<-lm(wage~I(educ^2),data=cps5_small)
summary(m2)

EDUC<-c(12,16)
margin<-numeric(2)
for (i in 1:2){
  margin[i]<-2*m2$coefficients[2]*EDUC[i]
}

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +  
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "red", se = FALSE) +  # 二次模型
  labs(title = "線性與二次回歸擬合", x = "EDUC", y = "WAGE") +
  scale_color_manual(name = "模型", values = c("Linear" = "blue", "Quadratic" = "red"))

