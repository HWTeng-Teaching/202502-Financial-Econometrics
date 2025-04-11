# 1.
load("commute5.rdata")
ls()

# 2.
data <- commute5

# (a) 
# 模型： TIME = β1 + β2*DEPART + β3*REDS + β4*TRAINS + e
model <- lm(time ~ depart + reds + trains, data)
summary(model)

# (b) 
ci <- confint(model)
print(ci)

# (c) 

beta3 <- coef(model)["reds"]
se_beta3 <- summary(model)$coefficients["reds", "Std. Error"]
t_stat_red <- (beta3 - 2) / se_beta3
p_value_red <- pnorm(t_stat_red)  # 左尾檢定 p-value
cat("檢定 (c) - 紅燈延遲：t-stat =", t_stat_red, "p-value =", p_value_red, "\n")


# (d) 

beta4 <- coef(model)["trains"]
se_beta4 <- summary(model)$coefficients["trains", "Std. Error"]
t_stat_train <- (beta4 - 3) / se_beta4
p_value_train <- 2 * (1 - pnorm(abs(t_stat_train)))  # 雙尾檢定 p-value
cat("檢定 (d) - 火車延遲：t-stat =", t_stat_train, "p-value =", p_value_train, "\n")

# (e) 

beta2 <- coef(model)["depart"]
se_beta2 <- summary(model)$coefficients["depart", "Std. Error"]
t_stat_depart <- (beta2 - 0.3333) / se_beta2
p_value_depart <- pnorm(t_stat_depart)  # 左尾檢定 p-value
cat("檢定 (e) - 出發時間影響：t-stat =", t_stat_depart, "p-value =", p_value_depart, "\n")

# (f) 

lh_test <- linearHypothesis(model, hypothesis.matrix = "trains - 3*reds = 0", test = "F")
print(lh_test)


# (g) 
L <- c(1, 30, 6, 1) 
est_time <- sum(coef(model) * L)
cov_mat <- vcov(model)
se_time <- sqrt(t(L) %*% cov_mat %*% L)

t_stat_time <- (est_time - 45) / se_time
p_value_time <- 1 - pnorm(t_stat_time)
cat("檢定 (g) - 準時到達：估計時間 =", est_time, "SE =", se_time,
    "t-stat =", t_stat_time, "p-value =", p_value_time, "\n")
