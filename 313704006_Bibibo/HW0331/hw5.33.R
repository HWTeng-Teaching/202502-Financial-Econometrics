library(POE5Rdata)
data(cps5_small)

#a. ln(WAGE) = β1 + β2EDUC + β3EDUC2 + β4EXPER + β5EXPER2 + β6(EDUC × EXPER) + e
mod = lm(log(wage)~educ+I(educ^2)+exper+I(exper^2)+educ*exper, data = cps5_small)
summary(mod)

#b. marginal effect ∂E[ln(WAGE)|EDUC, EXPER]/∂EDUC
#β2 + 2*β3*EDUC + β6*EXPER
b2 = coef(mod)[2]
b3 = coef(mod)[3]
b6 = coef(mod)[6]

#c. marginal effect in part (b) for all observations
row = nrow(cps5_small)
ME_educ = c()
for (i in 1:row){
  ME_educ[i] = b2 + 2*b3*cps5_small[i,2] + b6*cps5_small[i,3]
}
hist(ME_educ, xlab = "mg effect of educ", main = "hist of mg effect of educ")
med = median(ME_educ)
p5 = quantile(ME_educ, probs = 0.05)
p95 = quantile(ME_educ, probs = 0.95)
result_df <- data.frame(P5 = p5, Median = med, P95 = p95)
rownames(result_df) <- "value"

#d. ∂E[ln(WAGE)|EDUC, EXPER]/∂EXPER
#β4 + 2*β5*EEPER + β6*EDUC
b4 = coef(mod)[4]
b5 = coef(mod)[5]
#e. marginal effect in part (d) for all observations
ME_exper = c()
for (i in 1:row){
  ME_exper[i] = b4 + 2*b5*cps5_small[i,2] + b6*cps5_small[i,3]
}
hist(ME_exper, xlab = "mg effect of exper", main = "hist of mg effect of exper")
med = median(ME_exper)
p5 = quantile(ME_exper, probs = 0.05)
p95 = quantile(ME_exper, probs = 0.95)
result_df = data.frame(P5 = p5, Median = med, P95 = p95)
rownames(result_df) = "value"

#f. H0: -β2 - 33β3 + 10β4 + 260β5 + 152β6 >= 0 
df = df.residual(mod)
coeff = coef(mod)
covv = vcov(mod)
h0 = c(0, -1, -33, 10, 260, 152)
est = sum(h0*coeff)
std = (t(h0)%*%covv%*%h0)^0.5
t = est/std
tc = qt(0.05, df, lower.tail = TRUE)
t < tc #non-reject H0

#g. H0: -β2 - 33β3 + 10β4 + 420β5 + 144β6 >= 0 
h0 = c(0, -1, -33, 10, 420, 144)
est = sum(h0*coeff)
std = (t(h0)%*%covv%*%h0)^0.5
t = est/std
tc = qt(0.05, df, lower.tail = TRUE)
t < tc #reject H0

#h. H0: 12β5 - 4β6 = 0
h0 = c(0, 0, 0, 0, 12, -4)
est = sum(h0*coeff)
std = (t(h0)%*%covv%*%h0)^0.5
t = est/std
tc = qt(0.05, df, lower.tail = TRUE)
t < tc # non-reject H0

#i. x = (-β4 - β6)/ 2β5 -11
est = (-b4-16*b6)/(2*b5) - 11
g4 = -1/(2*b5)
g5 = (b4+16*b6)/(2*b5^2)
g6 = -8/b5
g = c(g4, g5, g6)
covv = vcov(mod)[4:6, 4:6]
varr = t(g)%*%covv%*%g
sd = varr^0.5
t = qt(0.025, df, lower.tail = FALSE)
lb = est - t * sd
ub = est + t * sd
interval = data.frame(lb = lb, ub = ub)
rownames(interval) = ""
