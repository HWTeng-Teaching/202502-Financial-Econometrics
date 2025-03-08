install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
data(cps5_small)
data <- cps5_small

# 檢查數據結構
str(data)
summary(data)

#取得統計數據
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



#b.

# 建立線性回歸模型
linear_model <- lm(wage ~ educ, data = data)

# 顯示回歸結果
summary(linear_model)


#c.

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


#d.


# 安裝並載入套件 (若尚未安裝)
install.packages("broom")
install.packages("dplyr")

library(broom)
library(dplyr)

# 原始的總體迴歸
model_all <- lm(wage ~ educ, data = data)

# 性別分組迴歸
model_male <- lm(wage ~ educ, data = subset(data, female == 0))
model_female <- lm(wage ~ educ, data = subset(data, female == 1))

# 種族分組迴歸
model_black <- lm(wage ~ educ, data = subset(data, black == 1))
model_non_black <- lm(wage ~ educ, data = subset(data, black == 0))

# 取得 tidy 格式結果
all_result <- tidy(model_all) %>% mutate(Group = "All")
male_result <- tidy(model_male) %>% mutate(Group = "Male")
female_result <- tidy(model_female) %>% mutate(Group = "Female")
black_result <- tidy(model_black) %>% mutate(Group = "Black")
non_black_result <- tidy(model_non_black) %>% mutate(Group = "Non-Black")

# 合併成一個完整表格
final_table <- bind_rows(all_result, male_result, female_result, black_result, non_black_result)

# 整理一下順序，方便查看
final_table <- final_table %>%
  select(Group, term, estimate, std.error, statistic, p.value) %>%
  arrange(term, Group)

# 顯示最終表格
print(final_table)


#e.

# 建立 EDUC 的平方項
data$educ2 <- data$educ^2

# 估計二次回歸模型
quad_model <- lm(wage ~ educ2, data = data)

# 顯示結果
summary(quad_model)

# 提取 α₂ 的估計值
alpha2 <- coef(quad_model)["educ2"]


# EDUC = 12年時的邊際效果
marginal_effect_12 <- 2 * alpha2 * 12

# EDUC = 16年時的邊際效果
marginal_effect_16 <- 2 * alpha2 * 16

marginal_effect_12
marginal_effect_16


#e.

library(ggplot2)

# 建立 EDUC 平方變數（若尚未建立）
data$educ2 <- data$educ^2

# 線性回歸模型（part b）
linear_model <- lm(wage ~ educ, data = data)
data$linear_fit <- predict(linear_model)

# 二次回歸模型（part e）
quad_model <- lm(wage ~ educ2, data = data)
data$quad_fit <- predict(quad_model)

# 繪圖
ggplot(data, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.4, color = "gray") + # 原始資料點
  geom_line(aes(y = linear_fit), color = "blue", size = 1.2, linetype = "dashed") + # 線性擬合線
  geom_line(aes(y = quad_fit), color = "red", size = 1.2) + # 二次擬合線
  labs(title = "Linear vs Quadratic Fit of Wage on Education",
       x = "Years of Education",
       y = "Hourly Wage") +
  theme_minimal() +
  scale_color_manual(values = c("Linear Fit" = "blue", "Quadratic Fit" = "red")) +
  theme(legend.position = "bottom")

