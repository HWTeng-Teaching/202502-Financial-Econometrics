library(POE5Rdata)
data("commute5")

#5.31(a)
model1 <- lm(time ~ depart + reds + trains, data = commute5)
summary(model1)

#5.31(b)
confint(model1,level=0.95)

#5.31(c)
alpha<-0.05
df<-model1$df.residual
b3<-coef(model1)["reds"]
seb3<-sqrt(vcov(model1)[3,3])
t_stat<-(b3-2)/seb3
tcr<-qt(alpha,df)
cat("t_stat",t_stat,'\n')
cat("tcr",tcr,'\n')
if(abs(t_stat)>tcr){
  cat("reject H0",'\n')
}else{
  cat("do not reject H0",'\n')
}

#5.31(d)
alpha<-0.1
df<-model1$df.residual
b4<-coef(model1)["trains"]
seb4<-sqrt(vcov(model1)[4,4])
t_stat<-(b4-3)/seb4
tcr<-qt(1-alpha/2,df)
cat("t_stat",t_stat,'\n')
cat("tcr",tcr,'\n')
if(abs(t_stat)>tcr){
  cat("reject H0",'\n')
}else{
  cat("do not reject H0",'\n')}

#5.31(e)
alpha<-0.05
df<-model1$df.residual
b2<-coef(model1)["depart"]
seb2<-sqrt(vcov(model1)[2,2])
t_stat<-(b2-1/3)/seb2
tcr<-qt(1-alpha,df)
cat("t_stat",t_stat,'\n')
cat("tcr",tcr,'\n')
if(abs(t_stat)>tcr){
  cat("reject H0",'\n')
}else{
  cat("do not reject H0",'\n')}

#5.31(f)
alpha<-0.05
df<-model1$df.residual
b3<-coef(model1)["reds"]
b4<-coef(model1)["trains"]
varb3<-vcov(model1)[3,3]
varb4<-vcov(model1)[4,4]
covb3b4<-vcov(model1)[4,3]
seb3b4<-sqrt(varb4+9*varb3-2*3*covb3b4)
t_stat<-(b4-3*b3)/seb3b4
tcr<-qt(1-alpha,df)
cat("t_stat",t_stat,'\n')
cat("tcr",tcr,'\n')
if(abs(t_stat)>tcr){
  cat("reject H0",'\n')
}else{
  cat("do not reject H0",'\n')}

#5.31(g)
alpha<-0.05
df<-model1$df.residual
b1<-coef(model1)["(Intercept)"]
b2<-coef(model1)["depart"]
b3<-coef(model1)["reds"]
b4<-coef(model1)["trains"]
varb1<-vcov(model1)[1,1]
varb2<-vcov(model1)[2,2]
varb3<-vcov(model1)[3,3]
varb4<-vcov(model1)[4,4]
covb1b2<-vcov(model1)[1,2]
covb2b3<-vcov(model1)[2,3]
covb3b4<-vcov(model1)[3,4]
covb1b3<-vcov(model1)[1,3]
covb1b4<-vcov(model1)[1,4]
covb2b4<-vcov(model1)[2,4]
se<-sqrt(varb1+900*varb2+36*varb3+varb4+2*30*covb1b2+2*6*covb1b3+2*1*covb1b4+2*180*covb2b3+2*30*covb2b4+2*6*covb3b4)
y_hat<-1*b1+30*b2+6*b3+b4
t_stat<-(y_hat-45)/se
tcr<-qt(1-alpha,df)

cat("t_stat",t_stat,'\n')
cat("tcr",tcr,'\n')

if(t_stat>tcr){
  cat("reject H0",'\n')
}else{
  cat("do not reject H0",'\n')
}
