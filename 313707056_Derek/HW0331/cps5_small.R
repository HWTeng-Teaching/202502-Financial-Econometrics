library(POE5Rdata)
library("ggplot2")
data("cps5_small")

model<-lm(log(wage)~educ+I(educ^2)+exper+I(exper^2)+I(educ*exper),data=cps5_small)
summary(model)

b1<-model$coefficients[1]
b2<-model$coefficients[2] 
b3<-model$coefficients[3] 
b4<-model$coefficients[4]
b5<-model$coefficients[5] 
b6<-model$coefficients[6] 

cps5_small['ME_educ']<-b2+2*b3*cps5_small$educ+b6*cps5_small$exper

ggplot(data=cps5_small,aes(x=ME_educ))+
  geom_histogram(binwidth =0.005,fill="blue",color="black" )

summary(cps5_small$ME_educ)
quantile(cps5_small$ME_educ, probs = c(0.05, 0.95))

cps5_small['ME_exper']<-b4+2*b5*cps5_small$exper+b6*cps5_small$educ

ggplot(data=cps5_small,aes(x=ME_exper))+
  geom_histogram(binwidth =0.002,fill="blue",color="black" )

summary(cps5_small$ME_exper)
quantile(cps5_small$ME_exper, probs = c(0.05, 0.95))

cov<-vcov(model)
coeff<-c(b1,b2,b3,b4,b5,b6)
a<-c(0,-1,-33,10,260,152)
var<-t(a) %*% cov %*% a
SE<-sqrt(var)

t<- t(a) %*% coeff/0.0215
qt(0.05,1194)
w

b<- c(0,-1,-33,10,420,144)
var_new<-t(b) %*% cov %*% b
SE_new<-sqrt(var_new)
t_new<-(t(b) %*% coeff)/SE_new

c<- c(0,0,0,0,12,-4)
var_new_c <- t(c) %*% cov %*% c
SE_new_c<-sqrt(var_new_c)
t_new_c<-(t(c) %*% coeff)/SE_new_c
qt(0.025,1194)

d4 <- (-1)/(2*b5)
d5 <- (b4+16*b6)/(2*b5^2)
d6 <- (-16)/(2*b5)
var_4 <- cov[4,4]
var_5 <- cov[5,5]
var_6 <- cov[6,6]
cov_45 <- cov[4,5]
cov_56 <- cov[5,6]
cov_46 <- cov[4,6]

var_x <- d4^2*var_4 + d5^2*var_5 + d6^2*var_6 + 2*d4*d5*cov_45 + 2*d5*d6*cov_56 + 2*d4*d6*cov_46
se_x <- sqrt(var_x)
qt(0.025,1194)
19.677-1.9*1.96
19.677+1.9*1.96
