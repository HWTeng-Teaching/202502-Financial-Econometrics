library(POE5Rdata)
data(cocaine)
?data
mod1<-lm(price~quant+qual+trend,data=cocaine)
summary(mod1)

#d
alpha<-0.05
df<-mod1$df.residual
b2<-coef(mod1)[["quant"]]
seb2<-sqrt(vcov(mod1)[2,2])

t_stat<-b2/seb2
tcr<-qt(alpha, df)

cat("t-statistic",t_stat)
cat("critical_value",tcr)

#e
alpha<-0.05
df<-mod1$df.residual
b3<-coef(mod1)[["qual"]]
seb3<-sqrt(vcov(mod1)[3,3])

t_stat<-b3/seb3
tcr<-qt(1-alpha,df)

cat("t-statistic",t_stat)
cat("critical_value",tcr)


