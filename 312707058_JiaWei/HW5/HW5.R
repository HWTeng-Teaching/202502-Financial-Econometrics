#-----------------C05Q03----------------

#-----------------C05Q03-----------------
n = 1200
k = 4
df = n-k

#(a)
Q1 = qt(1-0.5099/2,df)
Q2 = 2.7648/5.7103
Q3 = 0.3695*-3.9376

SSR = 46221.62
Q5 = sqrt(SSR/df)
SST = (n-1) * 6.39547^2
Q4 = 1 - (SSR/SST)
cat("The t-statistic for b1: ", Q1)
cat("The std for b2: ", Q2)
cat("The estimate for b3: ", Q3)
cat("R-squared: ", Q4)
cat("σ_hat: ", Q5)

#(c)
upr = -0.1503+0.0235*qt(1-0.05/2,df)
lwr = -0.1503-0.0235*qt(1-0.05/2,df)
cat("95%CI of β4: [", upr,", ",lwr,"]")

#(e)
t = abs((Q3+2)/0.3695)
print(1-pt(t,df))
#-----------------C05Q23-----------------
rm(list = ls())

library(POE5Rdata)
data(cocaine)
mod = lm(price~quant+qual+trend,data = cocaine)
n = 56
k = 4
#(b)
print(mod)
#(c)
print(summary(mod)[["r.squared"]])
#(d)
b2 = sum[["coefficients"]][2,1]
se_b2 = sum[["coefficients"]][2,2]
t = (b2 - 0)/se_b2
df = n-k
p_value = pt(t,df)
p_value
#(e)
b3 = sum[["coefficients"]][3,1]
se_b3 = sum[["coefficients"]][3,2]
t = (b3 - 0)/se_b3
df = n-k
p_value = 1-pt(t,df)
p_value
#(f)
sum[["coefficients"]][4,1]
