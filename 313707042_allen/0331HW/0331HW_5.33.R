library(POE5Rdata)
data(cps5_small)
?data

#a
mod1<-lm(log(wage)~educ+I(educ^2)+exper+I(exper^2)+I(educ*exper),data=cps5_small)
summary(mod1)

#c
b2<-coef(mod1)["educ"]
b3<-coef(mod1)["I(educ^2)"]
b6<-coef(mod1)["I(educ * exper)"]
ME_educ<-b2+2*b3*cps5_small$educ+b6*cps5_small$exper

hist(ME_educ,
     xlab="ME of EDUC",
     col="blue", border="white",
     breaks=30)
median_ME <- median(ME_educ)
p5_ME <- quantile(ME_educ, 0.05)
p95_ME <- quantile(ME_educ, 0.95)

cat("Median:",median_ME)
cat("5th Percentile:",p5_ME)
cat("95th Percentile:",p95_ME)

#e
b4<-coef(mod1)["exper"]
b5<-coef(mod1)["I(exper^2)"]
b6<-coef(mod1)["I(educ * exper)"]
ME_exper<-b4+2*b5*cps5_small$exper+b6*cps5_small$educ

hist(ME_exper,
     xlab="ME of EXPER",
     col="blue", border="white",
     breaks=30)
median_ME<-median(ME_exper)
p5_ME<-quantile(ME_exper,0.05)
p95_ME<-quantile(ME_exper,0.95)

cat("Median:",median_ME)
cat("5th Percentile:",p5_ME)
cat("95th Percentile:",p95_ME)

#f
x_david <- c(1, 17, 17^2, 8, 8^2, 17 * 8)
x_svet  <- c(1, 16, 16^2, 18, 18^2, 16 * 18)
c_vec <- x_svet - x_david

b <- coef(mod1)
vcov_mat <- vcov(mod1)
diff_est <- sum(c_vec * b)
se_diff <- sqrt(t(c_vec) %*% vcov_mat %*% c_vec)

t_stat <- diff_est / se_diff

alpha <- 0.05
df <- mod1$df.residual
t_crit <- qt(alpha, df)

cat("t-statistic =", round(t_stat, 4), "\n")
cat("t-critical (5%) =", round(t_crit, 4), "\n")

if (t_stat<t_crit) {
  cat("→ Reject H0: David's expected log-wage is significantly greater.\n")
} else {
  cat("→ Do NOT reject H0: No significant evidence David earns more.\n")
}

#g
exper_d_new <- exper_d + 8  
exper_s_new <- exper_s + 8

x_david_new <- c(1, 17, 17^2, 16, 16^2, 17 * 16)
x_svet_new  <- c(1, 16, 16^2, 26, 26^2, 16 * 26)

c_vec_new <- x_svet_new - x_david_new
b <- coef(mod1)
vcov_mat <- vcov(mod1)

diff_est_new <- sum(c_vec_new * b)
se_diff_new <- sqrt(t(c_vec_new) %*% vcov_mat %*% c_vec_new)
t_stat_new <- diff_est_new / se_diff_new
t_crit <- qt(0.05, df = mod1$df.residual)


cat("New t-statistic:", round(t_stat_new, 4), "\n")
cat("Critical value:", round(t_crit, 4), "\n")

if (t_stat_new < t_crit) {
  cat("→ Reject H0: David’s expected log-wage is significantly lower.\n")
} else {
  cat("→ Do not reject H0: No significant evidence David earns less.\n")
}

#h
c_vec <- c(0, 0, 0, 0, 12, -4)  

b <- coef(mod1)
vcov_mat <- vcov(mod1)

diff_est <- sum(c_vec * b)
se_diff <- sqrt(t(c_vec) %*% vcov_mat %*% c_vec)
t_stat <- diff_est / se_diff
df <- mod1$df.residual
t_crit <- qt(1 - 0.05 / 2, df)  # 5% 雙尾檢定

cat("t-stat =", round(t_stat, 4), "\n")
cat("t-critical =", round(t_crit, 4), "\n")

if (abs(t_stat) > t_crit) {
  cat("→ Reject H0: The marginal effects are significantly different.\n")
} else {
  cat("→ Do NOT reject H0: No significant difference.\n")
}

#i
b4 <- coef(mod1)["exper"]
b5 <- coef(mod1)["I(exper^2)"]
b6 <- coef(mod1)["I(educ * exper)"]

A <- b4 + 16 * b6
x_star <- -A / (2 * b5)

df_db4 <- -1 / (2 * b5)
df_db5 <- A / (2 * b5^2)
df_db6 <- -16 / (2 * b5)

grad <- c(df_db4, df_db5, df_db6)

vcov_mat <- vcov(mod1)
sub_vcov <- vcov_mat[c("exper", "I(exper^2)", "I(educ * exper)"), 
                     c("exper", "I(exper^2)", "I(educ * exper)")]

var_xstar <- t(grad) %*% sub_vcov %*% grad
se_xstar <- sqrt(var_xstar)

alpha <- 0.05
df <- mod1$df.residual
t_crit <- qt(1 - alpha / 2, df)
lower <- x_star - t_crit * se_xstar
upper <- x_star + t_crit * se_xstar

jill_years <- 11
x_relative <- x_star - jill_years
lower_relative <- lower - jill_years
upper_relative <- upper - jill_years

cat("x* =", round(x_star, 3), "\n")
cat("SE(x*) =", round(se_xstar, 3), "\n")
cat("95% CI for x*: [", round(lower, 2), ",", round(upper, 2), "]\n")
cat("Years until marginal effect becomes negative: ", round(x_relative, 3), "\n")
cat("95% CI for years from now: [", round(lower_relative, 2), ",", round(upper_relative, 2), "]\n")
