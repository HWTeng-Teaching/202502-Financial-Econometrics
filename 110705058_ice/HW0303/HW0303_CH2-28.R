url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
download.file(url, destfile = "cps5_small.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("cps5_small.rdata")

str(cps5_small)  # 查看結構
head(cps5_small)  # 查看前幾筆數據

summary(cps5_small$wage)
summary(cps5_small$educ)

ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of WAGE",
       x = "Hourly Wage ($)",
       y = "Frequency") +
  theme_minimal()

ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of EDUC",
       x = "Years of Education",
       y = "Frequency") +
  theme_minimal()

wage_model <- lm(wage ~ educ, data = cps5_small)
summary(wage_model)

cps5_small$residuals <- residuals(wage_model)
ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  labs(title = "Residuals vs. EDUC",
       x = "Years of Education",
       y = "Residuals") +
  theme_minimal()

summary(cps5_small)
data_male <- subset(cps5_small, female == 0)
data_female <- subset(cps5_small, female == 1)
data_black <- subset(cps5_small, black == 1)
data_white <- subset(cps5_small, black == 0)

model_male <- lm(wage ~ educ, data = data_male)
model_female <- lm(wage ~ educ , data = data_female)
model_black <- lm(wage ~ educ , data = data_black)
model_white <- lm(wage ~ educ , data = data_white)

summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)

get_equation <- function(model, group) {
  intercept <- round(coef(model)[1], 4)
  educ_coef <- round(coef(model)[2], 4)
  
  cat(group, "回歸方程:\n")
  cat("wage =", intercept, "+", educ_coef, "* educ\n\n")
}

get_equation(model_male, "男性")
get_equation(model_female, "女性")
get_equation(model_black, "黑人")
get_equation(model_white, "白人")


# Quadratic Regression
cps5_small$educ2 <- cps5_small$educ^2 
quad_model <- lm(wage ~ educ2, data = cps5_small)
summary(quad_model)

# 提取回歸係數
alpha_2 <- coef(quad_model)["educ2"]

# 邊際影響
marginal_effect_12 <- 2 * alpha_2 * 12
marginal_effect_16 <- 2 * alpha_2 * 16

cat("marginal_effect（EDUC = 12）:", marginal_effect_12, "\n")
cat("marginal_effect響（EDUC = 16）:", marginal_effect_16, "\n")

cps5_small$linear_fit <- predict(wage_model, newdata = cps5_small)
cps5_small$quadratic_fit <- predict(quad_model, newdata = cps5_small)
ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.5, color = "gray") + 
  geom_line(aes(y = linear_fit), color = "blue", linetype = "dashed", size = 1.2) + 
  geom_line(aes(y = quadratic_fit), color = "red", size = 1.2) + 
  labs(title = "Comparison of Linear and Quadratic Regression Models",
       x = "Years of Education",
       y = "Wage",
       caption = "Blue Dashed: Linear Model, Red: Quadratic Model") +
  theme_minimal()
