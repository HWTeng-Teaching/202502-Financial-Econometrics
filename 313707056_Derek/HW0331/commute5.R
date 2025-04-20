library(POE5Rdata)

data('commute5')

model<-lm(time~.,data = commute5)
summary(model)

coefficients_interval<-confint(model,level = 0.95)
qt(0.05,245)
qt(0.95,245)
cov<-vcov(model)

b1<-model$coefficients[1]
b2<-model$coefficients[2]
b3<-model$coefficients[3]
b4<-model$coefficients[4]

a<-c(1,30,6,1)
var_linear_comb <- t(a) %*% cov %*% a
se<-sqrt(var_linear_comb)
t<-(b1+30*b2+6*b3+b4-45)/se
