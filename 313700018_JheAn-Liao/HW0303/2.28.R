#A.
# 🌟 清空環境
rm(list = ls())

# 🌟 載入必要套件
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 🔗 下載並載入 cex5_small 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(cps5_small)

#Summary Statistic
summary_stats <- cps5_small %>%
  summarise(
    Mean_wage = mean(wage, na.rm = TRUE),
    Median_wage = median(wage, na.rm = TRUE),
    SD_wage = sd(wage, na.rm = TRUE),
    Min_wage = min(wage, na.rm = TRUE),
    Max_wage = max(wage, na.rm = TRUE),
    Mean_educ = mean(educ, na.rm = TRUE),
    Median_educ = median(educ, na.rm = TRUE),
    SD_educ = sd(educ, na.rm = TRUE),
    Min_educ = min(educ, na.rm = TRUE),
    Max_educ = max(educ, na.rm = TRUE)
  )


# 顯示摘要統計結果
print(summary_stats)
# 存取中位數為獨立變數
Median_wage <- summary_stats$Median_wage
#繪製wage直方圖
ggplot(cps5_small, aes(x = wage))+
  geom_histogram(fill = "red", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "Histogram of wage", x = 'Earnings per hour', y = "Frequency") +
  geom_vline(xintercept = Median_wage, color = "blue", linetype = "solid", size = 1)
  theme_minimal()
#繪製EDUC直方圖
ggplot(cps5_small, aes(x = educ))+
  geom_histogram(fill = "red", color = "black", bins = 20, alpha = 0.7) +
  labs(title = "Histogram of education", x = 'Years of education', y = "Frequency") +
  theme_minimal()
#Discussion
#工資分布右偏（可能有極端高收入者）
#平均工資為 23.64 美元/小時，但中位數為 19.3 美元/小時，顯示工資分布可能是右偏
#部分極端高收入者拉高了平均數，但大部分人的工資其實低於 23.64 美元/小時

#B.
model <- lm(wage ~ educ, data = cps5_small)
summary(model)
#提取迴歸係數
intercept <- coef(model)[1]
slope <- coef(model)[2]
regression_eq <- paste0("Wage = ", round(intercept,2),"+"
                        ,round(slope,2),"*income")
# 設定標示回歸公式的位置
x_pos <- quantile(cps5_small$wage, 0.2, na.rm = TRUE)
y_pos <- quantile(cps5_small$educ, 0.9, na.rm = TRUE)

#繪製散布圖與迴歸線
ggplot(cps5_small,aes(x = educ, y = wage)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +
  annotate("text", x = x_pos, y = y_pos, label = regression_eq,
           size = 5, color = "black") +
  labs(title = "Linear regression : Wage vs Education", x = "Education",y = "Wage" ) +
  theme_minimal()
# Discussion
#教育 (Education) 影響薪資 (Wage)	每增加 1 年教育，薪資平均增加 0.09 美元，但影響幅度很小。
#薪資變異大，影響因素可能不只教育	即使教育年數相同，工資可能仍然有很大差異，表示需要考慮其他變數。
#高教育者薪資分布較廣	高等教育者（16 年以上）薪資範圍變大，可能需要考慮 非線性模型。

#C.
#計算殘差
cps5_small <- cps5_small %>%
  mutate(residual= resid(model))
#繪製殘差圖
ggplot(cps5_small, aes(x = educ, y = residual)) +
  geom_point(color = "blue", alpha = 0.6, size = 1) + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 1) +
  labs(title = "Residual Plot: Wage vs Education", 
       x = "Years of Education", 
       y = "Residuals") +
  theme_minimal()

#Discussion
#殘差呈現斜直線，隨教育年數增加殘差隨之增加。
#高教育者薪資變異較大	存在異質變異數問題
#可能遺漏變數（Omitted Variable Bias）	薪資不僅受教育影響，應加入 exper（工作經驗）、female（性別）等變數進行多變數回歸分析。

#D.
male_model <- lm(wage ~ educ, data = cps5_small %>% filter(female == 0))
summary(male_model)

female_model <- lm(wage ~ educ, data = cps5_small %>% filter(female == 1))
summary(female_model)

white_model <- lm(wage ~ educ, data = cps5_small %>% filter(black == 0))
summary(white_model)

black_model <- lm(wage ~ educ, data = cps5_small %>% filter(black == 1))
summary(black_model)

# Discussion
#組別	截距	  斜率  R².    F 統計量   P 值	        標準誤
#男性 -8.28	  2.38	0.1927	160       p < 2.2e-16	  0.1881
#女性 -16.60  2.66	0.2764	200.9     p < 2.2e-16	  0.1876
#白人 -10.47	2.42	0.2072	285.7     p < 2.2e-16	  0.143
#黑人 -6.25	  1.92	0.1846	23.32     p = 4.788e-06	0.3983

#在給定黑人的情況下斜率較小，即黑人受教育年數對收入的增幅相較其他組別小，考慮到職場歧視機會不均等問題

#E.
#創建平方項

quad_model <- lm(wage ~ I(educ^2), data = cps5_small)
cps5_small$cps5_small_prdct <- predict(quad_model)
ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_line(aes(y = cps5_small_prdct), color = "red", size = 1) +
  labs(title = "Quadratic Model: Educ_sq VS wage", x = "Educ_sq", y = "Wage") +
  theme_minimal()

#Discussion
#截距由負轉正且異質變異數問題可能解決

#計算Marginal Effecct
alpha_1 = coef(quad_model)[1]
alpha_2 = coef(quad_model)[2]
ME_12 = 2 * alpha_2 * 12 
ME_16 = 2 * alpha_2 * 16

data_frame(
  Education_Level = c(12,16),
  Marginal_Level  = c(ME_12, ME_16)
)
summary(model)
summary(quad_model)

#F.
linear_model = lm(wage ~ educ, data = cps5_small)
quadratic_model = lm(wage ~ I(educ^2), data = cps5_small)

#計算擬合值
cps5_small$cps5_linear <- predict(linear_model)
cps5_small$cps5_quadratic <- predict(quadratic_model)

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "blue", size = 1, alpha = 0.6) +
  geom_line(aes(y = cps5_linear), color = "red",size = 1 ) +
  geom_line(aes(y = cps5_quadratic), color = "orange",size = 1 ) +
  labs(title = "Comparison of Linear and Quadratic Regression",
       x = "Years of education",
       y = "Wage"
      ) +
  theme_minimal()
#比較兩模型
anova(linear_model, quadratic_model)
AIC(linear_model, quadratic_model)
BIC(linear_model, quadratic_model)
#橘色（二次函數）較符合實際情況，截距項不應為負，但透過ANOVA,AIC,BIC比較，quadratic model
#僅稍優於linear model
