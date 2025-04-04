install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata")

library(POE5Rdata)
data(cps5_small)
data <- cps5_small

str(data)  # Check data
summary(data)  

summary(data$wage)
summary(data$educ)

mean(data$wage, na.rm = TRUE)      # 平均值
sd(data$wage, na.rm = TRUE)        # 標準差
median(data$wage, na.rm = TRUE)    # 中位數
quantile(data$wage, probs = c(0.25, 0.75), na.rm = TRUE) # 第1與第3四分位數

mean(data$educ, na.rm = TRUE)      # 教育年數的平均值
sd(data$educ, na.rm = TRUE)        # 教育年數的標準差
median(data$educ, na.rm = TRUE)    # 教育年數的中位數
quantile(data$educ, probs = c(0.25, 0.75), na.rm = TRUE) # 第1與第3四分位數


# 繪製 WAGE 和 EDUC 直方圖
library(ggplot2)

ggplot(data, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "WAGE 直方圖", x = "時薪（美元）", y = "頻數") +
  theme_minimal()

ggplot(data, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "EDUC 直方圖", x = "受教育年數", y = "頻數") +
  theme_minimal()
----------------------------------------------------------------------
#(B)
# 建立線性回歸模型
linear_model <- lm(wage ~ educ, data = data)

# 顯示回歸結果
summary(linear_model)

-----------------------------------
  #(C)
# 建立線性回歸模型 (若尚未建立)
model <- lm(wage ~ educ, data = data)

# 計算並儲存殘差
data$residuals <- residuals(model)

# 查看前幾筆殘差
head(data$residuals)

#繪製殘差圖
library(ggplot2)

ggplot(data, aes(x = educ, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. EDUC",
       x = "Years of Education (EDUC)",
       y = "Residuals") +
  theme_minimal()

---------------------------------------------------------------------
#(D)
install.packages("broom")
install.packages("dplyr")

library(broom)
library(dplyr)

# original overall regression
model_all <- lm(wage ~ educ, data = data)

# Gender grouped regression
model_male <- lm(wage ~ educ, data = subset(data, female == 0))
model_female <- lm(wage ~ educ, data = subset(data, female == 1))

# race group regression
model_black <- lm(wage ~ educ, data = subset(data, black == 1))
model_non_black <- lm(wage ~ educ, data = subset(data, black == 0))

# Get results in tidy format
all_result <- tidy(model_all) %>% mutate(Group = "All")
male_result <- tidy(model_male) %>% mutate(Group = "Male")
female_result <- tidy(model_female) %>% mutate(Group = "Female")
black_result <- tidy(model_black) %>% mutate(Group = "Black")
non_black_result <- tidy(model_non_black) %>% mutate(Group = "Non-Black")

# Merge into a complete table
final_table <- bind_rows(all_result, male_result, female_result, black_result, non_black_result)

# Organize the order 
final_table <- final_table %>%
  select(Group, term, estimate, std.error, statistic, p.value) %>%
  arrange(term, Group)

# Indicate the final result
print(final_table)

-------------------------------------------------------------------
#(E)
# 建立 EDUC 的平方項
data$educ2 <- data$educ^2

quad_model <- lm(wage ~ educ2, data = data)
summary(quad_model)

# 提取 α₂ 的估計值
alpha2 <- coef(quad_model)["educ2"]


# EDUC = 12年時的邊際效果
marginal_effect_12 <- 2 * alpha2 * 12

# EDUC = 16年時的邊際效果
marginal_effect_16 <- 2 * alpha2 * 16

marginal_effect_12
marginal_effect_16

--------------------------------------------------------------------
#(f)
  
# 線性回歸模型（part b）
linear_model <- lm(wage ~ educ, data = data)
data$linear_resid <- residuals(linear_model)
linear_SSE <- sum(data$linear_resid^2)

# 二次回歸模型（part e）
data$educ2 <- data$educ^2
quad_model <- lm(wage ~ educ2, data = data)
data$quad_resid <- residuals(quad_model)
quad_SSE <- sum(data$quad_resid^2)

# 顯示SSE結果
linear_SSE
quad_SSE

---------------------------------------------------------
#(補充)
  # 建立線性回歸模型 (part b)
  linear_model <- lm(wage ~ educ, data = data)

# 建立二次回歸模型 (part e)
# 假設已經有 data$educ2 <- data$educ^2
quad_model <- lm(wage ~ educ2, data = data)

# 取得模型的擬合值
data$linear_fitted <- fitted(linear_model)
data$quad_fitted   <- fitted(quad_model)

# 繪製散佈圖 (wage 對 educ)
plot(
  data$educ, data$wage,
  pch = 16,                   # 散點符號
  col = "black",
  xlab = "教育年數 (educ)",
  ylab = "工資 (wage)",
  main = "線性模型與二次模型的擬合比較"
)

# 在圖上加入線性回歸模型擬合線
# abline() 適用於「wage ~ educ」形式的線性模型
abline(linear_model, col = "blue", lwd = 2)

# 在圖上加入二次回歸模型擬合曲線
# 需要將 x 座標排序，才可畫出平滑的曲線
sorted_educ <- sort(data$educ)
quad_fit_sorted <- fitted(quad_model)[order(data$educ)]
lines(sorted_educ, quad_fit_sorted, col = "red", lwd = 2)

# 加入圖例
legend(
  "topleft",
  legend = c("線性回歸", "二次回歸"),
  col = c("blue", "red"),
  lwd = 2
)
