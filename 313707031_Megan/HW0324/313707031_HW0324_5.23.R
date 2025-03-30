library(POE5Rdata)
data("cocaine")

#5.23(b)
model_lm <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model_lm)

#5.23(d)
alpha<-0.05
df<-model_lm$df.residual
b2<-coef(model_lm)[["quant"]]
seb2<-sqrt(vcov(model_lm)[2,2])

t_stat<-b2/seb2
tcr<-qt(alpha, df)

cat("t-statistic",t_stat, "\n")
cat("critical_value",tcr, "\n")

#5.23(e)
alpha<-0.05
df<-model_lm$df.residual
b3<-coef(model_lm)[["qual"]]
seb3<-sqrt(vcov(model_lm)[3,3])

t_stat<-b3/seb3
tcr<-qt(1-alpha,df)

cat("t-statistic",t_stat, "\n")
cat("critical_value",tcr, "\n")
