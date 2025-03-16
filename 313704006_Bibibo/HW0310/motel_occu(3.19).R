library(POE5Rdata)
data("motel")
x = motel$comp_pct
y = motel$motel_pct
tm = motel$time

?plot
plot(tm, y, col="red", pch=20, xlab = "time", ylab = "occupation rate")
par(new=TRUE)
plot(tm, x, col="green", pch=20, xlab = "", ylab = "", axes=FALSE)
legend("bottomleft", c("motel", "comp"), col = c("red", "green"), pch = 20)

# MOTEL_PCT = β1 + β2COMP_PCT + e
# 95% interval estimate for the parameter β2
alpha = 0.05
ln_model = lm(y ~ x, data = motel)
b1 = coef(ln_model)[1]
b2 = coef(ln_model)[2]
df = df.residual(ln_model) # 23 (25-2)
s_ln_model = summary(ln_model)
seb2 = coef(s_ln_model)[2, 2]
tc = qt(alpha/2, df, lower.tail = FALSE)
lowb = b2-tc*seb2  # lower bound 0.445
upb = b2+tc*seb2   # upper bound 1.284
plot(x, y, xlab = "comp", ylab = "motel", pch=20)
abline(b1, b2, col = "red")

#90% interval estimate MOTEL_PCT, given that COMP_PCT = 70
alpha = 0.1
tc = qt(alpha/2, df, lower.tail = FALSE)
vcov(ln_model) #Variance-Covariance Matrix
COMP_PCT = 70
varb1 = vcov(ln_model)[1, 1]
varb2 = vcov(ln_model)[2, 2]
covb1b2 = vcov(ln_model)[1, 2]
vary = varb1 + COMP_PCT^2 *varb2 + 2*COMP_PCT*covb1b2
se_y = sqrt(vary)
est_y = b1 + b2*COMP_PCT
lowy = est_y - tc*se_y #77.382
upy = est_y + tc*se_y #86.467

# H0 :β2 ≤0 versus H1 :β2 >0
c = 0
t = (b2-c)/seb2 #4.265
p = pt(t, df, lower.tail = FALSE) # 0.00015 < 0.01 reject H0
rr = qt(0.01, df, lower.tail = FALSE) #t > 2.4999

# H0∶β2 = 1 against the alternative hypothesis H1∶β2 ≠ 1
c = 1
t = (b2-c)/seb2 #-0.668
p = 2*pt(t, df, lower.tail = TRUE) #0.511>0.01 non-reject H0
rr = qt(0.005, df, lower.tail = TRUE) # t<-2.807 or t>2.807

#residuals
ln_r = resid(ln_model)
plot(tm, ln_r, main="ln_mod_r",
     xlab="time", ylab="Residuals", pch=20)
abline(v = c(17, 23), col = "red")

