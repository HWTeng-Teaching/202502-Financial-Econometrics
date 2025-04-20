library(POE5Rdata)
data(commute5)
?data
#a
mod1<-lm(time~depart+reds+trains,data=commute5)
summary(mod1)

#b
confint(mod1,level=0.95)

#c
alpha<-0.05
df<-mod1$df.residual
b3<-coef(mod1)["reds"]
seb3<-sqrt(vcov(mod1)[3,3])
t_stat<-(b3-2)/seb3
tcr<-qt(alpha,df)
cat("t_stat",t_stat)
cat("tcr",tcr)
if(abs(t_stat)>tcr){
  cat("reject H0")
}else{
  cat("do not reject H0")
}

#d
alpha<-0.1
df<-mod1$df.residual
b4<-coef(mod1)["trains"]
seb4<-sqrt(vcov(mod1)[4,4])
t_stat<-(b4-3)/seb4
tcr<-qt(1-alpha/2,df)
cat("t_stat",t_stat)
cat("tcr",tcr)
if(abs(t_stat)>tcr){
  cat("reject H0")
  }else{
  cat("do not reject H0")}

#e
alpha<-0.05
df<-mod1$df.residual
b2<-coef(mod1)["depart"]
seb2<-sqrt(vcov(mod1)[2,2])
t_stat<-(b2-1/3)/seb2
tcr<-qt(1-alpha,df)
cat("t_stat",t_stat)
cat("tcr",tcr)
if(abs(t_stat)>tcr){
  cat("reject H0")
}else{
  cat("do not reject H0")}

#f
alpha<-0.05
df<-mod1$df.residual
b3<-coef(mod1)["reds"]
b4<-coef(mod1)["trains"]
varb3<-vcov(mod1)[3,3]
varb4<-vcov(mod1)[4,4]
covb3b4<-vcov(mod1)[4,3]
seb3b4<-sqrt(varb4+9*varb3-2*3*covb3b4)
t_stat<-(b4-3*b3)/seb3b4
tcr<-qt(1-alpha,df)
cat("t_stat",t_stat)
cat("tcr",tcr)
if(abs(t_stat)>tcr){
  cat("reject H0")
}else{
  cat("do not reject H0")}

#g
alpha<-0.05
df<-mod1$df.residual
b1<-coef(mod1)["(Intercept)"]
b2<-coef(mod1)["depart"]
b3<-coef(mod1)["reds"]
b4<-coef(mod1)["trains"]
varb1<-vcov(mod1)[1,1]
varb2<-vcov(mod1)[2,2]
varb3<-vcov(mod1)[3,3]
varb4<-vcov(mod1)[4,4]
covb1b2<-vcov(mod1)[1,2]
covb2b3<-vcov(mod1)[2,3]
covb3b4<-vcov(mod1)[3,4]
covb1b3<-vcov(mod1)[1,3]
covb1b4<-vcov(mod1)[1,4]
covb2b4<-vcov(mod1)[2,4]
se<-sqrt(varb1+900*varb2+36*varb3+varb4+2*30*covb1b2+2*6*covb1b3+2*1*covb1b4+2*180*covb2b3+2*30*covb2b4+2*6*covb3b4)
y_hat<-1*b1+30*b2+6*b3+b4
t_stat<-(y_hat-45)/se
tcr<-qt(1-alpha,df)

cat("t_stat",t_stat)
cat("tcr",tcr)

if(t_stat>tcr){
  cat("reject H0")
}else{
  cat("do not reject H0")
}



