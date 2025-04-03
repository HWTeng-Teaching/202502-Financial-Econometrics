library(POE5Rdata)
data(commute5)

#a. results and interpret each of the coefficient estimates
lr = lm(time~depart+reds+trains, data = commute5)
summary(lr)

#b. 
df = 249 - 4
est = coef(lr)
std = summary(lr)$coefficients[, 2]
vcovar = vcov(lr)
tc = qt(0.025, df, lower.tail = FALSE)
lb = est - tc*std
ub = est + tc*std

interval <- matrix(0, nrow = length(lb), ncol = 2) 

for (i in 1:length(lb)) { 
  interval[i, 1] <- lb[i]  
  interval[i, 2] <- ub[i]  
}
rownames(interval) = c("intercept", "depart", "reds", "trains")
colnames(interval) = c("lb", 'ub')

#c. H0: Beta3 >= 2
t = (coef(lr)[3]-2)/summary(lr)$coefficients[3, 2]
tc = qt(0.05, df, lower.tail = TRUE)
t < tc #reject H0

#d. H0: Beta4 = 3
t = (coef(lr)[4]-3)/summary(lr)$coefficients[4, 2]
tc = qt(0.05, df, lower.tail = FALSE)
t > tc #non-reject H0

#e. H0: 30Beta2 >= 10
#H0: Beta2 >= 1/3
t = (coef(lr)[2]-1/3)/summary(lr)$coefficients[2, 2]
tc = qt(0.05, df, lower.tail = TRUE)
t < tc #non-reject H0

#f. H0: Beta4 >= 3Beta3
#H0: Beta4 - 3Beta3 >= 0
coeff = c(0, 0, -3, 1)
estL = sum(coeff * est)
varL = t(coeff)%*%vcovar%*%coeff
stdL = varL^0.5
t = estL/stdL
tc = qt(0.05, df, lower.tail = TRUE)
t < tc #reject H0

#g. H0: B1 + 30B2 + 6B3 + B4 <= 45
coeff = c(1, 30, 6, 1)
estL = sum(coeff * est)
varL = t(coeff)%*%vcovar%*%coeff
stdL = varL^0.5
t = (estL - 45)/stdL
tc = qt(0.05, df, lower.tail = FALSE)
t > tc #non-reject H0 證據不夠說會遲到

#h. H0: B1 + 30B2 + 6B3 + B4 >= 45
#Ha: B1 + 30B2 + 6B3 + B4 < 45
coeff = c(1, 30, 6, 1)
estL = sum(coeff * est)
varL = t(coeff)%*%vcovar%*%coeff
stdL = varL^0.5
t = (estL - 45)/stdL
tc = qt(0.05, df, lower.tail = TRUE)
t < tc #reject H0 可以準時
