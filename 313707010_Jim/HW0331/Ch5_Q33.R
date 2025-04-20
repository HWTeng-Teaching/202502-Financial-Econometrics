library(POE5Rdata)

data("cps5_small")
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model)

# (c)
b_educ <- coef(model)["educ"]
b_educ2 <- coef(model)["I(educ^2)"]
b_edexp <- coef(model)["I(educ * exper)"]

marginal_effect_educ <- b_educ + 2 * b_educ2 * cps5_small$educ + b_edexp * cps5_small$exper

cps5_small$marginal_educ <- marginal_effect_educ

hist(marginal_effect_educ,
     breaks = 30,
     col = "skyblue",
     main = "Marginal Effect of EDUC on ln(WAGE)",
     xlab = "∂ln(wage)/∂educ")

quantile(marginal_effect_educ, probs = c(0.05, 0.5, 0.95))

# (e)
b_exper  <- coef(model)["exper"]
b_exper2 <- coef(model)["I(exper^2)"]
b_edexp  <- coef(model)["I(educ * exper)"]

marginal_effect_exper <- b_exper + 2 * b_exper2 * cps5_small$exper + b_edexp * cps5_small$educ

cps5_small$marginal_exper <- marginal_effect_exper

hist(marginal_effect_exper,
     breaks = 30,
     col = "lightgreen",
     main = "Marginal Effect of EXPER on ln(WAGE)",
     xlab = "∂ln(wage)/∂exper")

quantile(marginal_effect_exper, probs = c(0.05, 0.5, 0.95))

# (f)
b <- coef(model)

svetlana_x <- c(1, 16, 16^2, 18, 18^2, 16*18)
logwage_svetlana <- sum(svetlana_x * b)

david_x <- c(1, 17, 17^2, 8, 8^2, 17*8)
logwage_david <- sum(david_x * b)

diff <- logwage_david - logwage_svetlana
diff
x_diff <- david_x - svetlana_x

vcov_matrix <- vcov(model)

se_diff <- sqrt(t(x_diff) %*% vcov_matrix %*% x_diff)

t_stat <- diff / se_diff

p_value <- pt(t_stat, df = df.residual(model), lower.tail = FALSE)
qt(0.95, df = 1194)
t_stat
p_value

# (g)
david_x_new <- c(1, 17, 17^2, 16, 16^2, 17*16)
svetlana_x_new <- c(1, 16, 16^2, 26, 26^2, 16*26)
x_diff_new <- david_x_new - svetlana_x_new

diff_new <- sum(x_diff_new * coef(model))

se_diff_new <- sqrt(t(x_diff_new) %*% vcov(model) %*% x_diff_new)

t_stat_new <- diff_new / se_diff_new

p_value_new <- pt(t_stat_new, df = df.residual(model), lower.tail = FALSE)
qt(0.05, df = 1194)
diff_new
se_diff_new
t_stat_new
p_value_new

# (h)
alpha = 0.05
tcr_two <- qt(1-alpha/2, model$df.residual)

A3 <- c(0, 0, 0, 0, 12, -4)
L3 <- as.numeric(t(A3) %*% coef(model))
se_L3 <- as.numeric(sqrt(t(A3) %*% vcov(model) %*% A3))
tva_L3 <- L3/se_L3
cat("t = ", tva_L3, "\n")
cat("critical value = ", tcr_two, "\n")


# (i)
b4 <- coef(model)["exper"]
b5 <- coef(model)["I(exper^2)"]
b6 <- coef(model)["I(educ * exper)"]

educ_val <- 16
current_exper <- 11
g <- -(b4 + educ_val*b6)/(2*b5) - current_exper
g

g4 <- -1/(2*b5)  
g5 <- (b4 + 16*b6)/(2*b5^2)  
g6 <- -16/(2*b5)  
grad <- c(g4, g5, g6)

v_sub <- vcov(model)[
  c("exper", "I(exper^2)", "I(educ * exper)"),
  c("exper", "I(exper^2)", "I(educ * exper)")
]

varg <- t(grad) %*% v_sub %*% grad
seg <- sqrt(varg)
cat(
  "var(g):", varg, "\n",
  "se(g):", seg, "\n"
)

lowbg <- g - tcr_two*seg
upbg <- g + tcr_two*seg
cat("Extra years:", g, " with 95% interval estimates [", lowbg, ", ", upbg, "]\n")
