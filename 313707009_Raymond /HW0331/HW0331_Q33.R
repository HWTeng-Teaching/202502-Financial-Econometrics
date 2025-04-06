library(POE5Rdata)
data('cps5_small')

data <- cps5_small
names(data) <- tolower(names(data))

# (a)建立模型
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = data)

summary(model)
# (c)教育邊際報酬
b <- coef(model)
marginal_educ <- b["educ"] + 2 * b["I(educ^2)"] * data$educ + b["I(educ * exper)"] * data$exper

# 繪製直方圖
hist(marginal_educ, main = "邊際教育效果分布", xlab = "∂E[ln(wage)]/∂educ")

# 分位數
quantile(marginal_educ, probs = c(0.05, 0.5, 0.95))
# (c)工作經驗邊際報酬
marginal_exper <- b["exper"] + 2 * b["I(exper^2)"] * data$exper + b["I(educ * exper)"] * data$educ

# 繪製直方圖
hist(marginal_exper, main = "邊際經驗效果分布", xlab = "∂E[ln(wage)]/∂exper")

# 分位數
quantile(marginal_exper, probs = c(0.05, 0.5, 0.95))

# (f) 比較 David 與 Svetlana 的預期薪資
x_david <- c(1, 17, 17^2, 8, 8^2, 17*8)
x_svet  <- c(1, 16, 16^2, 18, 18^2, 16*18)

X_diff <- x_david - x_svet
var_cov <- vcov(model)
var_diff <- t(X_diff) %*% var_cov %*% X_diff
se_diff <- sqrt(var_diff)

diff <- sum(X_diff * b)
t_val <- diff / se_diff
p_val <- pt(t_val, df = model$df.residual)

print(diff)
print(t_val)
print(p_val)
#(g) 多8年經驗在比較
x_david_new <- c(1, 17, 17^2, 16, 16^2, 17*16)
x_svet_new  <- c(1, 16, 16^2, 26, 26^2, 16*26)

X_diff_new <- x_david_new - x_svet_new
diff_new <- sum(X_diff_new * b)
var_diff_new <- t(X_diff_new) %*% var_cov %*% X_diff_new
se_diff_new <- sqrt(var_diff_new)

t_val_new <- diff_new / se_diff_new
p_val_new <- pt(t_val_new, df = model$df.residual)

print(diff_new)
print(t_val_new)
print(p_val_new)
# (h) 邊際效果是否相同
# 各自的邊際效應表示式
wendy_expr <- b["exper"] + 2 * b["I(exper^2)"] * 17 + b["I(educ * exper)"] * 12
jill_expr  <- b["exper"] + 2 * b["I(exper^2)"] * 11 + b["I(educ * exper)"] * 16

# 差值
diff_marginal <- wendy_expr - jill_expr

# 建立相對應的 X 向量差值 (針對 b4, b5, b6)
X_diff_marginal <- c(0, 0, 0, 1, 2*(17 - 11), 12 - 16)

se_marginal <- sqrt(t(X_diff_marginal) %*% var_cov %*% X_diff_marginal)
t_val_marginal <- diff_marginal / se_marginal
p_val_marginal <- 2 * pt(-abs(t_val_marginal), df = model$df.residual)

print(diff_marginal)
print(t_val_marginal)
print(p_val_marginal)
# (i) 邊際效果信賴區間
# 取 Jill 教育水準 = 16，解一元二次方程式
a <- 2 * b["I(exper^2)"]
b1 <- b["exper"]
c <- b["I(educ * exper)"] * 16

# 解方程式： a * x + b1 + c = 0
critical_x <- (-b1 - c) / a
print(critical_x)
# 計算變異數：透過 chain rule
g <- c(0, 0, 0, -1/a, -2*(b1 + c)/a^2, -16/a)
se_x <- sqrt(t(g) %*% var_cov %*% g)

# 95% 信賴區間
ci <- critical_x + c(-1, 1) * qt(0.975, df = model$df.residual) * se_x
print(ci)





