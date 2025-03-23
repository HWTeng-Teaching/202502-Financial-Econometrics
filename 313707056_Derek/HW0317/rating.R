library(ggplot2)
#Model 1
exper<-0:30
rating_hat<-64.289+0.990*exper

data<-data.frame(rating=rating_hat,exper=exper)

m1<-lm(rating~exper,data=data)

fitted_values_m1<-predict(m1)

ggplot(data=data,aes(x=exper,y=rating))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y~x,se=FALSE)+
  labs(title="Model 1")
# Model 2
exper_2<-1:30
rating_hat_2<-39.464+15.312*log(exper_2)
data_2<-data.frame(rating=rating_hat_2,exper=exper_2)

m2<-lm(rating~exper,data=data_2)

ggplot(data=data_2,aes(x=exper,y=rating))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y~log(x),se=FALSE)+
  labs(title="Model 2")

