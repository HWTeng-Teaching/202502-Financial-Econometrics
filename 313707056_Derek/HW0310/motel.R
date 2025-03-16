library(tidyverse)
library(POE5Rdata)
library(dplyr)
library(ggplot2)
data(motel)
?motel
ggplot(data=motel,aes(x=time))+
  geom_line(aes(y=motel_pct,color="Motel Occupancy"))+
  geom_line(aes(y=comp_pct,color="Competitors Occupancy"))+
  labs(x="Time",y="Occupancy Percentage")

m1<-lm(motel_pct~comp_pct,data=motel)
summary(m1)

confint(m1,level=0.95)

new_data<-data.frame(comp_pct=70)
predict(m1,new_data,interval = "confidence",level=0.9)

b1<-m1$coefficients[1]
b2<-m1$coefficients[2]
SE_beta2<-summary(m1)$coefficients["comp_pct","Std. Error"]
# c 
t_beta2=(b2-0)/SE_beta2
qt(0.99,m1$df.residual)
1-pt(t_beta2,m1$df.residual)

# d H0:beta2=1
t_beta2_1=(b2-1)/SE_beta2
qt(0.005,m1$df.residual)
pt(t_beta2_1,m1$df.residual)

SSE<-sum(m1$residuals^2)
ggplot(data=motel,aes(x=time))+
  geom_point(aes(x=time,y=m1$residuals))+
  labs(x="Time",y="residual")

#23.92+0.833*qt(0.025,212)
#28.6-0.8164*qt(0.025,984)
#se<-(3.29^2+(16^2)*0.24^2-2*16*0.761)^(1/2)
#se_urban<-(2.27^2+(16^2)*0.16^2-2*16*0.345)^(1/2)