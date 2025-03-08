library(POE5Rdata)
data("cps5_small")
x = cps5_small$educ
y = cps5_small$wage
female = cps5_small$female
black = cps5_small$black

summary(x)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0    12.0    14.0    14.2    16.0    21.0 
hist(x, main = "Histogram of education", xlab = "education")
summary(y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.94   13.00   19.30   23.64   29.80  221.10 
hist(x, main = "Histogram of wage", xlab = "wage")

#ln regression (WAGE = β1 + β2EDUC + e)
ln_model = lm(y~x, data = cps5_small)
coef(ln_model)
#(Intercept)           x 
#-10.399959    2.396761 

#residuals
residual = resid(ln_model)
plot(x, residual, xlab = "education")

#compare M, F, Black, White
male_x = cps5_small$educ[female==0]
male_y = cps5_small$wage[female==0]
m_ln_model = lm(male_y~male_x, data = cps5_small)
m_ln_model
#(Intercept)       male_x  
#-8.285        2.378
female_x = cps5_small$educ[female==1]
female_y = cps5_small$wage[female==1]
f_ln_model = lm(female_y~female_x, data = cps5_small)
f_ln_model
#(Intercept)     female_x  
#-16.603        2.659 
black_x = cps5_small$educ[black==1]
black_y = cps5_small$wage[black==1]
b_ln_model = lm(black_y~black_x, data = cps5_small)
b_ln_model
#(Intercept)      black_x  
#-6.254        1.923 
white_x = cps5_small$educ[black==0]
white_y = cps5_small$wage[black==0]
w_ln_model = lm(white_y~white_x, data = cps5_small)
w_ln_model
#(Intercept)      white_x  
#-10.475        2.418  

#quadratic (WAGE = α1 + α2EDUC^2 + e)
qd_model = lm(y~I(x^2), data = cps5_small)
qd_model
#(Intercept)       I(x^2)  
#4.91648      0.08913 

#marginal effect of another year of education on wage (2*α2*EDUC)
α1 = coef(qd_model)[1]
α2 = coef(qd_model)[2]
edu = c(12, 16)
marginal_effect = 2*α2*edu
marginal_effect
#2.139216 2.852288

plot(x, y, main = "wage vs edu", xlab = "edu", ylab = "wage")
abline(ln_model, col="red")
curve(α1+α2*(x^2), from = min(x), to = max(x), col="green", add = TRUE)
legend("topleft", c("linear","quadratic"), col = c("red","green"),
       lwd = 2, lty = 1)
#lwd現粗度。 lty線長相 （虛線）
