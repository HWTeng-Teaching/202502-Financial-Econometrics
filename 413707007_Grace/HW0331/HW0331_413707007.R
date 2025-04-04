#5.31
load("C:\\Users\\thaol\\Downloads\\commute5.rdata")
commute5
mod1 <- lm(time~depart+reds+trains, data=commute5)
smod1 <- summary(mod1)
smod1
df <- mod1$df.residual
N <- nobs(mod1)
qt(0.95, df)
confint(mod1, level = 0.95)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
b3 <- coef(mod1)[[3]]
b4 <- coef(mod1)[[4]]
varb1 <- vcov(mod1)[1,1]
varb2 <- vcov(mod1)[2,2]
varb3 <- vcov(mod1)[3,3]
varb4 <- vcov(mod1)[4,4]
covb3b4 <- vcov(mod1)[3,4]
covb3b4
varg_f <- varb4 + 9*varb3 -2*3*covb3b4
seg_f <- sqrt(varg_f)
tf <- (b4 - 3*b3) / seg_f
tf
# Define the vector A
A <- as.vector(c(1, 30, 6, 1))

# Extract covariance values from the covariance matrix of the model mod1
cov_matrix <- vcov(mod1)

# Calculate the variance of the linear combination using the delta method formula
var_g <- t(A) %*% cov_matrix %*% A

# Print the result
var_g
seg_g <- sqrt(var_g)
tg <- (b1 + 30*b2 + 6*b3 + b4 - 45) / seg_g
tg



#5.33
load("C:\\Users\\thaol\\Downloads\\cps5_small.rdata")
cps5_small
model <- lm(log(Wage) ~ educ + I(educ^2) + exper + I(exper^2) + educ:exper, data = cps5_small)

# Display the summary of the model
summary(model)
beta2 <- coef(model)[2]  # Coefficient for EDUC
beta3 <- coef(model)[3]  # Coefficient for EDUC^2 (or EDUC if that's the variable)
beta4 <- coef(model)[4]  # Coefficient for EXPER
beta5 <- coef(model)[5]  # Coefficient for EXPER^2
beta6 <- coef(model)[6]  # Coefficient for EXPER
M_EDUC <- beta2 + 2 * beta3 * cps5_small$educ + beta6 * cps5_small$exper
M_EDUC

library(tseries)
hist(M_EDUC, 
     main = "Histogram of M_EDUC", 
     xlab = "M_EDUC", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)
jb_test <- jarque.bera.test(M_EDUC)
print(jb_test)
quantile(M_EDUC, probs = c(0.05, 0.5, 0.95))
summary(M_EDUC)

M_EXPER <- beta4 + 2 * beta5 * cps5_small$exper + beta6 * cps5_small$educ
hist(M_EXPER, 
     main = "Histogram of M_EXPER", 
     xlab = "M_EDUC", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)
jb_test_e <- jarque.bera.test(M_EXPER)
print(jb_test_e)
quantile(M_EXPER, probs = c(0.05, 0.5, 0.95))
summary(M_EXPER)

#f
# Define the vector F
F <- as.vector(c(0, 1, 33, -10, -260, -152))
# Extract covariance values from the covariance matrix of the model mod1
cov_matrix_533 <- vcov(model)

# Calculate the variance of the linear combination using the delta method formula
var_533f <- t(F) %*% cov_matrix_533 %*% F

# Print the result
var_533f
seg_533f <- sqrt(var_533f)
t533f <- ( beta2 + 33*beta3 - 10*beta4 - 260*beta5 - 152*beta6) / seg_533f
t533f
t_cr <- qt(0.05,1194)


#g.
G <- as.vector(c(0, -1, -33, 10, 420, 144))
var_533g <- t(G) %*% cov_matrix_533 %*% G
seg_533g <- sqrt(var_533g)
t533g <- ( -1* beta2 - 33*beta3 + 10*beta4 + 420*beta5 + 144*beta6) / seg_533g
t533g


#h.
H <- as.vector(c(0, 0,0,0, 12, -4))
var_533h <- t(H) %*% cov_matrix_533 %*% H
seg_533h <- sqrt(var_533h)
t533h <- ( 12*beta5 -4*beta6) / seg_533h
t533h
t_crh <- qt(0.975,1194)

#i
# gi = -(b3 + 16*b4)/(2*b4)-11

g_i <- -(beta4 + 16*beta6)/(2*beta5) - 11
g_i
g4 <- -1/(2*beta5)  
g5 <- (beta4 + 16*beta6)/(2*beta5^2)  
g6 <- -16/(2*beta5)  
g4
g5
g6
var_4 <- cov_matrix_533[4,4]
var_5 <- cov_matrix_533[5,5]
var_6 <- cov_matrix_533[6,6]
cov_45 <- cov_matrix_533[4,5]
cov_56 <- cov_matrix_533[5,6]
cov_46 <- cov_matrix_533[4,6]

# Var(g_i)
varg_i <-   g4^2*var_4 + g5^2*var_5 + g6^2*var_6 + 2*g4*g5*cov_45 + 2*g5*g6*cov_56 + 2*g4*g6*cov_46
seg_i <- sqrt(varg_i)
seg_i
lowbg <- g_i - t_crh*seg_i
upbg <- g_i + t_crh*seg_i
lowbg
upbg
