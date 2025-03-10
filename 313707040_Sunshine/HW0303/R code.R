library(POE5Rdata)
library(ggplot2)
# CH2Q17
data(collegetown)

# 檢查數據結構
str(collegetown)
summary(collegetown)
head(collegetown)
names(collegetown) 

# 繪製散點圖
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  labs(title = "House Price vs House Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)")

# 線性回歸
linear_model <- lm(price ~ sqft, data = collegetown)
summary(linear_model)

# 繪製線性回歸擬合線
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Linear Regression Fit",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)")

# 二次回歸
quadratic_model <- lm(price ~  I(sqft^2), data = collegetown)
summary(quadratic_model)

# 計算邊際效應 (SQFT = 20，即 2000 平方英尺)
sqft_2000 <- 20
marginal_effect <-  coef(quadratic_model)[2] * (21^2 - 20^2)
marginal_effect

# 繪製二次回歸擬合曲線
plot <- ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, col = "red") +
  labs(title = "Quadratic Regression Fit",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)")

# 計算在 sqft = 20 處的切線
slope_tangent <- 2 * coef(quadratic_model)[2] * sqft_2000
intercept_tangent <- predict(quadratic_model, newdata = data.frame(sqft = sqft_2000)) - slope_tangent * sqft_2000

tangent_line <- data.frame(sqft = c(min(collegetown$sqft), max(collegetown$sqft)))
tangent_line$price <- intercept_tangent + slope_tangent * tangent_line$sqft

# 加入切線到圖形
plot + geom_line(data = tangent_line, aes(x = sqft, y = price), col = "blue", linetype = "dashed")

# 計算彈性 (sqft = 20)
elasticity <- (2 * coef(quadratic_model)[2] * sqft_2000) * (sqft_2000 / predict(quadratic_model, newdata = data.frame(sqft = sqft_2000)))
elasticity

# 計算殘差
residuals_linear <- residuals(linear_model)
residuals_quadratic <- residuals(quadratic_model)

# 繪製殘差圖
ggplot(data.frame(sqft = collegetown$sqft, Residuals = residuals_linear), aes(x = sqft, y = Residuals)) +
  geom_point() +
  labs(title = "Residual Plot for Linear Model",
       x = "House Size (hundreds of square feet)",
       y = "Residuals")

ggplot(data.frame(sqft = collegetown$sqft, Residuals = residuals_quadratic), aes(x = sqft, y = Residuals)) +
  geom_point() +
  labs(title = "Residual Plot for Quadratic Model",
       x = "House Size (hundreds of square feet)",
       y = "Residuals")
# 計算 SSE
sse_linear <- sum(residuals_linear^2)
sse_quadratic <- sum(residuals_quadratic^2)
list(SSE_Linear = sse_linear, SSE_Quadratic = sse_quadratic)


# CH2Q25
data(cex5_small)

# (a) 繪製 FOODAWAY 的直方圖並計算摘要統計
ggplot(cex5, aes(x = foodaway)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of FOODAWAY",
       x = "FOODAWAY (dollars)",
       y = "Income")

summary(cex5$foodaway)
quantile(cex5$foodaway, probs = c(0.25, 0.75))

# (b) 計算不同學歷家庭的 FOODAWAY 平均值與中位數
advanced_degree <- cex5$foodaway[cex5$advanced == 1]
college_degree <- cex5$foodaway[cex5$college == 1]
no_degree <- cex5$foodaway[cex5$advanced == 0 & cex5$college == 0]

mean(advanced_degree, na.rm = TRUE)
median(advanced_degree, na.rm = TRUE)
mean(college_degree, na.rm = TRUE)
median(college_degree, na.rm = TRUE)
mean(no_degree, na.rm = TRUE)
median(no_degree, na.rm = TRUE)

# (c) 繪製 ln(FOODAWAY) 的直方圖
cex5 <- cex5[cex5$foodaway > 0, ]  # 只保留 foodaway > 0 的觀察值
cex5$ln_foodaway <- log(cex5$foodaway)

ggplot(cex5, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.5, fill = "red", alpha = 0.5) +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "ln(FOODAWAY)",
       y = "Income")

summary(cex5$ln_foodaway)

# (d) 執行線性回歸 ln(FOODAWAY) ~ INCOME
reg_model <- lm(ln_foodaway ~ income, data = cex5)
summary(reg_model)

# (e) 繪製 ln(FOODAWAY) 對 INCOME 的散點圖，並加入回歸線
ggplot(cex5, aes(x = income, y = ln_foodaway)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression of ln(FOODAWAY) on INCOME",
       x = "Income (in $100 units)",
       y = "ln(FOODAWAY)")

# (f) 計算殘差並繪製殘差圖
residuals_data <- data.frame(income = cex5$income, Residuals = residuals(reg_model))
ggplot(residuals_data, aes(x = income, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot",
       x = "Income (in $100 units)",
       y = "Residuals")

# CH2Q28
data(cps5_small)

# (a) 檢視 WAGE 和 EDUC 變數的摘要統計與直方圖
summary(cps5_small$wage)
summary(cps5_small$educ)

ggplot(cps5_small, aes(x = wage)) + geom_histogram(bins = 30, fill = "blue", alpha = 0.7) + 
  ggtitle("Histogram of WAGE") + xlab("WAGE") + ylab("Frequency")

ggplot(cps5_small, aes(x = educ)) + geom_histogram(bins = 30, fill = "red", alpha = 0.7) + 
  ggtitle("Histogram of EDUC") + xlab("EDUC") + ylab("Frequency")

# (b) 線性回歸分析
linear_model <- lm(wage ~ educ, data = cps5_small)
summary(linear_model)

# (c) 殘差計算與繪製
cps5_small$residuals <- residuals(linear_model)

ggplot(cps5_small, aes(x = educ, y = residuals)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs educ") + xlab("educ") + ylab("Residuals")

# (d) 針對不同性別與種族進行回歸分析
male_model <- lm(wage ~ educ, data = subset(cps5_small, female == 0))
female_model <- lm(wage ~ educ, data = subset(cps5_small, female == 1))

black_model <- lm(wage ~ educ, data = subset(cps5_small, black == 1))
white_model <- lm(wage ~ educ, data = subset(cps5_small, black == 0))

summary(male_model)
summary(female_model)
summary(black_model)
summary(white_model)

# (e) 二次回歸分析
quad_model <- lm(wage ~ I(educ^2), data = cps5_small)
summary(quad_model)

# 計算邊際效應
alpha1 <- coef(quad_model)[1]
alpha2 <- coef(quad_model)[2]

marginal_effect_12 <-  2 * alpha2 * 12
marginal_effect_16 <-  2 * alpha2 * 16

marginal_effect_12
marginal_effect_16

# (f) 繪製線性與二次回歸的擬合曲線
cps5_small$linear_fit <- predict(linear_model, newdata = cps5_small)
cps5_small$quad_fit <- predict(quad_model, newdata = cps5_small)

ggplot(cps5_small, aes(x = educ, y = wage)) + 
  geom_point(alpha = 0.5) +
  geom_line(aes(y = linear_fit), color = "blue", linetype = "dashed") +
  geom_line(aes(y = quad_fit), color = "red") +
  ggtitle("Linear vs Quadratic Fit") +
  xlab("educ") + ylab("wage") +
  theme_minimal()
