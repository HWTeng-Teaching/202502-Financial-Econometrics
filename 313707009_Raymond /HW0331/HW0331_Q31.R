if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("car")
install.packages("carData")

library(POE5Rdata)
data('commute5')

data <- commute5

model <- lm(time ~ depart + reds + trains, data = data)

# (a) 顯示回歸結果
summary(model)
# (b) 信賴區間
confint(model, level = 0.95)
# (c) 紅燈是否多停兩分鐘(beta3)
reds_coef <- summary(model)$coefficients["reds", "Estimate"]
reds_se <- summary(model)$coefficients["reds", "Std. Error"]
# 單邊檢定
t_c <- (reds_coef - 2) / reds_se
p_c <- pt(t_c, df = model$df.residual)  # 單尾
t_c; p_c
# (d)是否因火車停超過三分鐘
trains_coef <- summary(model)$coefficients["trains", "Estimate"]
trains_se <- summary(model)$coefficients["trains", "Std. Error"]

t_d <- (trains_coef - 3) / trains_se
p_d <- 2 * pt(-abs(t_d), df = model$df.residual)  # 雙尾
t_d; p_d
# (e) 不同時間點出發的差距
depart_coef <- summary(model)$coefficients["depart", "Estimate"]
depart_se <- summary(model)$coefficients["depart", "Std. Error"]

t_e <- (30 * depart_coef - 10) / (30 * depart_se)
p_e <- pt(t_e, df = model$df.residual)
t_e; p_e
# (f) 火車延遲是否為紅燈的三倍
library(car)
linearHypothesis(model, "trains - 3*reds = 0")

# (g)、(h)  檢定在 7:00AM 出發，遇到 6 個紅燈、1 次火車，是否能準時（<= 75 分鐘）
new_data <- data.frame(depart = 30, reds = 6, trains = 1)
pred <- predict(model, newdata = new_data, se.fit = TRUE)
t_g <- (pred$fit - 75) / pred$se.fit
p_g <- 1 - pt(t_val, df = model$df.residual)
print(t_g)
print(p_g)
p_h <- pt(t_g, df = model$df.residual)  # 改為左尾檢定
p_h





