#-----------------C05Q06-----------------
rm(list = ls())
n = 63
b <- matrix(c(2,3,-1))
cov <- matrix(c(3,-2,1,-2,4,0,1,0,3),nrow=3,ncol=3)
df = n-3
#(a)
c = matrix(c(0,1,0))
b_Q1 = t(c) %*% b
se_Q1 = sqrt(t(c) %*% cov %*% c)
t = (b_Q1-0)/se_Q1
cv = qt(1-0.05/2,df)
cat("t-statistic:",t,"\ncritical value: ±",cv)
#(b)
c = matrix(c(1,2,0))
b_Q2 = t(c) %*% b
se_Q2 = sqrt(t(c) %*% cov %*% c)
t = (b_Q2-5)/se_Q2
cv = qt(1-0.05/2,df)
cat("t-statistic:",t,"\ncritical value: ±",cv)
#(c)
c = matrix(c(1,-1,1))
b_Q3 = t(c) %*% b
se_Q3 = sqrt(t(c) %*% cov %*% c)
t = (b_Q3-4)/se_Q3
cv = qt(1-0.05/2,df)
cat("t-statistic:",t,"\ncritical value: ±",cv)

#-----------------C05Q31-----------------
rm(list = ls())
library(POE5Rdata)
data("commute5")
n = nrow(commute5)
#(a)
mod = lm(time ~ depart+reds+trains,data = commute5)
mod

#(b)
df = n - 4
sum = summary(mod)
b = sum[["coefficients"]][1:4,1]
se_b = sum[["coefficients"]][1:4,2]
cov = vcov(mod)
t = qt(1-0.05/2,df)
t
for (i in 1:4){
cat("b",i,":[",b[i]-t*se_b[i],
    ",",b[i]+t*se_b[i],"]\n")
}
#(c)
t = (b[3] - 2)/se_b[3]
cv = qt(0.05,df)
cat("t-statistic:",t,"\ncritical value:",cv)
#(d)
t = (b[4] - 2)/se_b[4]
cv = qt(1-0.1/2,df)
cat("t-statistic:",t,"\ncritical value: ±",cv)
#(e)
t = (b[2] - 1/3)/se_b[2]
cv = qt(0.05,df)
cat("t-statistic:",t,"\ncritical value:",cv)

#(f)
c = matrix(c(0,0,-3,1))
b_f = t(c) %*% b
se_f = sqrt(t(c) %*% cov %*% c)
t = (b_f-0)/se_f
cv = qt(0.05,df)
cat("t-statistic:",t,"\ncritical value: ",cv)

#(g)
#method 1
c = matrix(c(1,30,6,1))
y = t(c) %*% b
se_y = sqrt(t(c) %*% cov %*% c)
t = (y-45)/se_y
cv = qt(1-0.05,df)
cat("t-statistic:",t,"\ncritical value:",cv)

#method 2
x = data.frame(depart=30,reds=6,trains=1)
CI <- predict(mod, newdata = x, interval = "confidence", level = 0.90)
cat("95% LWR:",CI[2],"\nreject region: [45,∞]")

#(h)
c = matrix(c(1,30,6,1))
y = t(c) %*% b
se_y = sqrt(t(c) %*% cov %*% c)
t = (y-45)/se_y
cv = qt(0.05,df)
cat("t-statistic:",t,"\ncritical value:",cv)
#-----------------C05Q33-----------------
rm(list = ls())
library(POE5Rdata)
data("cps5_small")
n = nrow(cps5_small)
mod = lm(I(log(wage)) ~ educ+I(educ^2)+exper+I(exper^2)+educ*exper,data = cps5_small)

df = n - 6
sum = summary(mod)
b = sum[["coefficients"]][1:6,1]
se_b = sum[["coefficients"]][1:6,2]
cov = vcov(mod)
#(a)
sum

#(b)
"b2+2*b3*EDUC+b6*EXPER"

#(c)
dy_dEDUC <- b[2]+2*b[3]*cps5_small$educ+b[6]*cps5_small$exper
hist(dy_dEDUC,
     xlab="Marginal Effect from EDUC",breaks = 50
)
q = quantile(dy_dEDUC, probs = c(0.05, 0.5, 0.95))
cat("5th: ",q[1]," median:",q[2], " 95th:",q[3])

#(d)
"b4+2*b5*EXPER+b6*EDUC"

#(e)
dy_dEXPER <- b[4]+2*b[5]*cps5_small$exper+b[6]*cps5_small$educ
hist(dy_dEXPER,
     xlab="Marginal Effect from EDUC",breaks = 50
)
q = quantile(dy_dEXPER, probs = c(0.05, 0.5, 0.95))
cat("5th: ",q[1]," median:",q[2], " 95th:",q[3])
#(f)
D = matrix(c(1,17,17*17,8,8*8,17*8))
S = matrix(c(1,16,16*16,18,18*18,16*18))
c = S-D
b_f = t(c) %*% b
se_f = sqrt(t(c) %*% cov %*% c)
t = (b_f-0)/se_f
cv = qt(0.05,df)
cat("t-statistic:",t,"\ncritical value: ",cv)
#(g)
D = matrix(c(1,17,17*17,(8+8),(8+8)*(8+8),17*(8+8)))
S = matrix(c(1,16,16*16,(18+8),(18+8)*(18+8),16*(18+8)))
c = S-D
b_g = t(c) %*% b
se_g = sqrt(t(c) %*% cov %*% c)
t = (b_g-0)/se_g
cv = qt(0.05,df)
cat("t-statistic:",t,"\ncritical value: ",cv)
#(h)
"b4+b5*2*EXPER+b6*EDUC"
W = matrix(c(0,0,0,1,2*17,12))
J = matrix(c(0,0,0,1,2*11,16))
c = W-J
b_h = t(c) %*% b
se_h = sqrt(t(c) %*% cov %*% c)
t = (b_h-0)/se_h
cv = qt(1-0.05/2,df)
cat("t-statistic:",t,"\ncritical value: ±",cv)

#(i)
EDUC = 16
"point estimate"
"b4+b5*2*EXPER+b6*EDUC = 0"
"EXPER = (-b4-b6*EDUC)/(2*b5)"
pe = (-b[4]-b[6]*16)/(2*b[5])
pe

"interval estimate"
"EXPER = (-b4-b6*EDUC)/(2*b5) ~ ?"
"The distribution of EXPER is not a linear combination of the estimators, 
 which causes its distribution to be unidentified. 
 We use the point estimate to calculate the interval of marginal effect(y)
 and then get the interval of EXPER by the formula"

"y = b4+b5*2*EXPER+b6*EDUC, (y-0)/se(y) ~ t"
"CI of y/se(y): ±t*se(y) "
"CI of EXPER  : (-b4-b6*EDUC±t*se(y))/(2*b5) = point estimate ± (t*se(y))/(2*b5)"

c = matrix(c(0,0,0,1,2*pe,16))
se_y = sqrt(t(c) %*% cov %*% c)
ie = c(pe-qt(1-0.05/2,df)*se_y/(2*b[5]),pe+qt(1-0.05/2,df)*se_y/(2*b[5]))
cat("EXPER interval: [",min(ie),",",max(ie),"]",
    "\nJill remains: [",min(ie)-11,",",max(ie)-11,"]")

"we can also use the delta method and get the same results"
g4 = (-1)/(2*b[5])
g5 = (b[4]+b[6]*EDUC)/(2*b[5]*b[5])
g6 = (-EDUC)/(2*b[5])

c = matrix(c(0,0,0,g4,g5,g6))
se_EXPER = sqrt(t(c) %*% cov %*% c)
t = qt(1-0.05/2,df)
cat("by delta method:\nEXPER interval: [",pe-t*se_EXPER,",",pe+t*se_EXPER,"]",
    "\nJill remains: [",pe-t*se_EXPER-11,",",pe+t*se_EXPER-11,"]")
