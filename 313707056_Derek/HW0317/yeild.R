library(POE5Rdata)
library(ggplot2)
library(tseries)
data("wa_wheat")
?wa_wheat
linear<-lm(northampton~time,data=wa_wheat)
summary(linear)
sse_linear<-sum(linear$residuals^2)

linear_log<-lm(northampton~log(time),data = wa_wheat)
summary(linear_log)
sse_linear_log<-sum(linear_log$residuals^2)

quadratic<-lm(northampton~I(time^2),data = wa_wheat)
summary(quadratic)
sse_quadratic<-sum(quadratic$residuals^2)

log_linear<-lm(log(northampton)~time ,data = wa_wheat)
summary(log_linear)
sse_log_linear<-sum(log_linear$residuals^2)

ggplot(data=wa_wheat,aes(x=time,y=northampton))+
  geom_point()+
  labs(y="yeild")+
  geom_smooth(method="lm",formula = y~x, se=FALSE,color="blue")+
  geom_smooth(method="lm",formula=y~log(x), se=FALSE,color="green")+
  geom_smooth(method='lm',formula = y~I(x^2), se=FALSE, color="orange")+
  geom_line(aes(y=exp(predict(log_linear))),col="purple",linewidth = 1,)+
  annotate("text",x=5,y=2.5,label="linear-linear",color="blue",size=10)+
  annotate("text",x=5,y=2.3,label="linear-log",color="green",size=10)+
  annotate("text",x=5,y=2.1,label="quadratic",color="orange",size=10)+
  annotate("text",x=5,y=1.9,label="log-linear",color="purple",size=10)
  

plot(linear)
plot(linear_log)
plot(quadratic)
plot(log_linear)

jarque.bera.test(linear$residuals)
jarque.bera.test(linear_log$residuals)
jarque.bera.test(quadratic$residuals)
jarque.bera.test(log_linear$residuals)

#LEVERAGE
leverage_values <- hatvalues(quadratic)
print(leverage_values)
n <- nrow(wa_wheat)
p <- length(coefficients(quadratic))
threshold_leverage <- 2 * p / n
which(leverage_values > threshold_leverage)  # 顯示高槓桿點的索引
#DFBETAS
dfbetas_values <- dfbetas(quadratic)
print(dfbetas_values)
which(abs(dfbetas_values) > 2/sqrt(n), arr.ind=TRUE)
#
dffits_values <- dffits(quadratic)
print(dffits_values)
threshold_dffits <- 2 * sqrt(p/n)
which(abs(dffits_values) > threshold_dffits)  # 顯示影響值較大的點

T<-data.frame(time=47)
predict(quadratic,newdata = T,interval = "predict",level=0.95)




