install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata")

library(POE5Rdata)
data(cps5_small)
data <- cps5_small

str(data)  # Check data
summary(data) 

model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = data)
summary(model)

-------------------------------------------------------------
#C
b <- coef(model)

# 計算邊際效果 for each observation
data$marginal_effect_educ <- b["educ"] + 2 * b["I(educ^2)"] * data$educ + b["I(educ * exper)"] * data$exper

# 描述統計量
quantiles <- quantile(data$marginal_effect_educ, probs = c(0.05, 0.5, 0.95))
print(quantiles)

# 繪製直方圖
hist(data$marginal_effect_educ, 
     main = "Marginal Effect of EDUC on log(WAGE)", 
     xlab = "Marginal Effect", 
     col = "green2", 
     border = "tan3")

--------------------------------------------
#E

# 計算 exper 的邊際效果：∂ln(wage)/∂exper
data$marginal_exper <- b["exper"] + 2 * b["I(exper^2)"] * data$exper + b["I(educ * exper)"] * data$educ

# 統計百分位數
quantile(data$marginal_exper, probs = c(0.05, 0.5, 0.95))

hist(data$marginal_exper,
     main = "Marginal Effect of EXPER on log(WAGE)",
     xlab = "Marginal Effect",
     col = "green3",
     border = "tan4",
     breaks = 30)

library(ggplot2)
ggplot(data, aes(x = marginal_exper)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Marginal Effect of EXPER on log(WAGE)",
       x = "Marginal Effect",
       y = "Density") +
  theme_minimal()
