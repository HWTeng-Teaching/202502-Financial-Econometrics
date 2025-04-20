#5.31
library(POE5Rdata)
data("commute5")
str(commute5)
#a
m1<-lm(time~depart+reds+trains,data=commute5)
summary(m1)
#b
confint(m1,level = 0.95)#計算信賴區間
#f
cov_matrix <- vcov(m1)
cov_matrix
c_vec <- c(0, 0, -3, 1)
(sef <- sqrt(t(c_vec) %*% cov_matrix %*% c_vec))
#g
new_data <- data.frame(depart = 30, reds = 6, trains = 1)
(predicted_TIME <- predict(m1, newdata = new_data, se.fit = TRUE))
c_vec1 <- c(1, 30, 6, 1)
(sef <- sqrt(t(c_vec1) %*% cov_matrix %*% c_vec1))

#5.33
data("cps5_small")
#a
m2<-lm(log(wage)~educ+I(educ^2)+ exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(m2)

#c
install.packages("dplyr")  
library(dplyr)
coef <- coef(m2)
cps5_small <- cps5_small %>%
  mutate(me_educ = coef["educ"] + 2 * coef["I(educ^2)"] * educ + coef["I(educ * exper)"] * exper)
quantile(cps5_small$me_educ, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$me_educ, main="Histogram of Marginal Effect of EDUC", 
     xlab="Marginal Effect of EDUC", col="lightblue", border="black",breaks = 40)

#e
cps5_small <- cps5_small %>%
  mutate(me_exper = coef["exper"] + 2 * coef["I(exper^2)"] * exper + coef["I(educ * exper)"] * educ)
quantile(cps5_small$me_exper, probs = c(0.05, 0.5, 0.95))
hist(cps5_small$me_exper, main="Histogram of Marginal Effect of EXPER", 
     xlab="Marginal Effect of EXPER", col="lightblue", border="black",breaks = 40)

#f
cov_matrix2 <- vcov(m2)
c_vec2 <- c(0, -1, -33, 10, 260, 152)
coef_column_vec <- matrix(coef(m2), ncol = 1)
(t <- t(c_vec2) %*% coef_column_vec)
(sef <- sqrt(t(c_vec2) %*% cov_matrix2 %*% c_vec2))

#g
c_vec3 <- c(0, -1, -33, 10, 420, 144)
(t1 <- t(c_vec3) %*% coef_column_vec)
(sef <- sqrt(t(c_vec3) %*% cov_matrix2 %*% c_vec3))


#h
c_vec4 <- c(0, 0, 0, 0, 12, -4)
(t2 <- t(c_vec4) %*% coef_column_vec)
(sef <- sqrt(t(c_vec4) %*% cov_matrix2 %*% c_vec4))

#i
beta_values <- coef(m2)
cov_matrix2 <- vcov(m2)

x <- (-beta_values["exper"] - 22 * beta_values["I(exper^2)"] - 16 * beta_values["I(educ * exper)"]) / (2 * beta_values["I(exper^2)"])
x

d_b4 <- -1 / (2 * beta_values["I(exper^2)"])
d_b5 <- (beta_values["exper"] + beta_values["I(educ * exper)"] * 16) / (2 * beta_values["I(exper^2)"]^2)
d_b6 <- -16 / (2 * beta_values["I(exper^2)"])

se <- sqrt(
  d_b4^2 * cov_matrix2["exper", "exper"] + 
    d_b5^2 * cov_matrix2["I(exper^2)", "I(exper^2)"] + 
    d_b6^2 * cov_matrix2["I(educ * exper)", "I(educ * exper)"] +
    2 * d_b4 * d_b5 * cov_matrix2["I(exper^2)", "exper"] +
    2 * d_b4 * d_b6 * cov_matrix2["exper", "I(educ * exper)"] +
    2 * d_b5 * d_b6 * cov_matrix2["I(exper^2)", "I(educ * exper)"]
)
se