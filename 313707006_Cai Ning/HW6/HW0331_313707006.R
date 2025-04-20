#5.31
library(POE5Rdata)
data("commute5")

#(a)
model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

#(b)
confint(model, level = 0.95)

#(c)
beta_reds <- coef(summary(model))["reds", "Estimate"]
se_reds <- coef(summary(model))["reds", "Std. Error"]
t_value_reds <- (beta_reds - 2) / se_reds
cat(t_value_reds)
qt(0.05, 245)

#(d)
beta_trains <- coef(summary(model))["trains", "Estimate"]
se_trains <- coef(summary(model))["trains", "Std. Error"]
t_value_trains <- (beta_trains - 3) / se_trains
cat(t_value_trains)
qt(0.05, 245)

#(e)
beta_depart <- coef(summary(model))["depart", "Estimate"]
se_depart <- coef(summary(model))["depart", "Std. Error"]
t_value_depart <- (beta_depart - 0.3333) / se_depart
cat(t_value_depart)
qt(0.05, 245)  

#(f)
beta_trains <- coef(model)["trains"]
beta_reds <- coef(model)["reds"]
#共變異數矩
vcov_matrix <- vcov(model)
c_vec <- c(0, 0, -3, 1)
sef <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec)
t_value_f <- (beta_trains - 3 * beta_reds) / sef
cat(t_value_f)
qt(0.05, 245) 

#(g)(h)

new_data <- data.frame(depart = 30, reds = 6, trains = 1)
predicted <- predict(model, newdata = new_data, se.fit = TRUE)
predicted_TIME <- predicted$fit
vcov_matrix <- vcov(model)
c_vec <- c(1, 30, 6, 1)
se_expected_time <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec)
t_value_g <- (predicted_TIME - 45) / se_expected_time
cat(t_value_g)
qt(0.95, 245) 


#5.33
library(POE5Rdata)
data("cps5_small")

#(a)
model2<- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model2)

#(b)#(c)
data_df <- cps5_small 
b2 <- coef(model2)["educ"]
b3 <- coef(model2)["I(educ^2)"]       
b6 <- coef(model2)["I(educ * exper)"] 
data_df$me_educ <- b2 + 2 * b3 * data_df$educ + b6 * data_df$exper

library(ggplot2)
ggplot(data_df, aes(x = me_educ)) +
  geom_histogram(binwidth = 0.004, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Marginal Effects of EDUC on ln(WAGE)",
       x = "Marginal Effect",
       y = "Frequency") +
  theme_minimal()

quantile(data_df$me_educ, probs = c(0.05, 0.5, 0.95))
min(data_df$me_educ)
max(data_df$me_educ)

#(d)(e)
b3 <- coef(model2)["exper"]
b4 <- coef(model2)["I(exper^2)"]
b5 <- coef(model2)["I(educ * exper)"]
data_df$me_exper <- b3 + 2 * b4 * data_df$exper + b5 * data_df$educ

library(ggplot2)
ggplot(data_df, aes(x = me_exper)) +
  geom_histogram(binwidth = 0.004, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Marginal Effects of EXPER on ln(WAGE)",
       x = "Marginal Effect",
       y = "Frequency") +
  theme_minimal()

quantile(data_df$me_exper, probs = c(0.05, 0.5, 0.95))
min(data_df$me_exper)
max(data_df$me_exper)

#(f)
b0 <- coef(model2)["(Intercept)"]
b1 <- coef(model2)["educ"]
b2 <- coef(model2)["I(educ^2)"]
b3 <- coef(model2)["exper"]
b4 <- coef(model2)["I(exper^2)"]
b5 <- coef(model2)["I(educ * exper)"]

david_logwage <- b0 + b1 * 17 + b2 * (17^2) + b3 * 8 + b4 * (8^2) + b5 * (17 * 8)
svetlana_logwage <- b0 + b1 * 16 + b2 * (16^2) + b3 * 18 + b4 * (18^2) + b5 * (16 * 18)
diff_logwage <- svetlana_logwage - david_logwage  

X_david <- c(1, 17, 17^2, 8, 8^2, 17*8)
X_svetlana <- c(1, 16, 16^2, 18, 18^2, 16*18)

var_beta <- vcov(model2)  
var_diff <- t(X_svetlana - X_david) %*% var_beta %*% (X_svetlana - X_david)  # 修正順序

se_diff <- sqrt(var_diff)
t_value <- diff_logwage / se_diff
cat(t_value)
qt(0.05,1194)

#(g)
david_logwage_new <- b0 + b1*17 + b2*(17^2) + b3*16 + b4*(16^2) + b5*(17*16)
svetlana_logwage_new <- b0 + b1*16 + b2*(16^2) + b3*26 + b4*(26^2) + b5*(16*26)
diff_logwage_new <- svetlana_logwage_new - david_logwage_new  

X_david_new <- c(1, 17, 17^2, 16, 16^2, 17*16)
X_svetlana_new <- c(1, 16, 16^2, 26, 26^2, 16*26)

var_diff_new <- t(X_svetlana_new - X_david_new) %*% var_beta %*% (X_svetlana_new - X_david_new)
se_diff_new <- sqrt(var_diff_new)

t_value_new <- diff_logwage_new / se_diff_new
cat(t_value_new)

#(h)
me_wendy <- b3 + 2 * b4 * 17 + b5 * 12
me_jill <- b3 + 2 * b4 * 11 + b5 * 16

diff_me <- me_wendy - me_jill

X_wendy <- c(0, 0, 0, 1, 2*17, 12)  
X_jill <- c(0, 0, 0, 1, 2*11, 16)

var_diff_me <- t(X_wendy - X_jill) %*% var_beta %*% (X_wendy - X_jill)
se_diff_me <- sqrt(var_diff_me)
t_value_me <- diff_me / se_diff_me
cat(t_value_me)
qt(0.975,1194)

#(i)
# 點估計，計算 g
educ_val <- 16
current_exper <- 11
g <- - (b3 + educ_val * b5) / (2 * b4) - current_exper

# 計算偏導數
g3 <- -1 / (2 * b4)  # 對 b3 偏導
g4 <- (b3 + educ_val * b5) / (2 * b4^2)  # 對 b4 偏導
g5 <- -educ_val / (2 * b4)  # 對 b5 偏導
grad <- c(g3, g4, g5)

# 提取協方差矩陣
v_sub <- vcov(model2)[
  c("exper", "I(exper^2)", "I(educ * exper)"),
  c("exper", "I(exper^2)", "I(educ * exper)")
]

# 計算 Var(g)
varg <- t(grad) %*% v_sub %*% grad
seg <- sqrt(varg)

# 計算 95% 置信區間
tcr_two <- qt(0.975, df = df.residual(model2))  # 取得 t 分佈 95% 臨界值
lowbg <- g - tcr_two * seg
upbg <- g + tcr_two * seg

# 輸出結果
cat(lowbg,upbg)
 