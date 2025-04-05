library(POE5Rdata)
POE5Rdata::commute5

#(a)
mod1 <- lm(time~depart+reds+trains,data = commute5)

s <- summary(mod1)
s
coefs <- round(s$coefficients[,1], 4)
ses   <- round(s$coefficients[,2], 4)


cat(paste0( #paste0用來把多個文字合併成一串,只寫paste不會有空格
  "TIME = ", coefs[1], 
  ifelse(coefs[2] >= 0, " + ", " - "), abs(coefs[2]), "DEPART", 
  ifelse(coefs[3] >= 0, " + ", " - "), abs(coefs[3]), "REDS",
  ifelse(coefs[4] >= 0, " + ", " - "), abs(coefs[4]), "TRAINS",
  "\n\n",  #這是你計算出來的 R-squared 值
  
  "(se)    ", paste("(", ses, ")", collapse = "  "), "\n" ))

#(b)
confint(mod1,level = 0.95)

#(f)
mod1 <- lm(time~depart+reds+trains,data = commute5)
coef_matrix <- vcov(mod1)
coef_matrix


#5.33
#(a)
library(POE5Rdata)
POE5Rdata::cps5_small
mod2 <- lm(log(wage)~educ+I(educ^2)+exper+I(exper^2)+educ*exper,data = cps5_small)
summary(mod2)
#(c)
# 載入必要的套件（如果需要 ggplot2）
library(ggplot2)

# 提取回歸係數
coefs <- coef(mod2)
beta1 <- coefs["educ"]           # educ 的係數
beta2 <- coefs["I(educ^2)"]      # educ^2 的係數
beta5 <- coefs["educ:exper"]     # educ*exper 的係數

# 從資料中提取 educ 和 exper
educ <- cps5_small$educ
exper <- cps5_small$exper

# 計算每個觀察值的邊際效應
marginal_effect <- beta1 + 2 * beta2 * educ + beta5 * exper

# 繪製直方圖（使用基本 hist）
hist(marginal_effect, 
     main = "Histogram of Marginal Effect of educ on log(wage)", 
     xlab = "Marginal Effect", 
     col = "lightblue", 
     breaks = 20)

# （可選）使用 ggplot2 繪製更美觀的直方圖
df <- data.frame(marginal_effect = marginal_effect)
ggplot(df, aes(x = marginal_effect)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Histogram of Marginal Effect of educ ", 
       x = "Marginal Effect", y = "Frequency") +
  theme_minimal()

summary(marginal_effect)
quantile(marginal_effect,c(0.05,0.5,0.95))


#(e)
# 載入必要的套件（如果使用 ggplot2）
library(ggplot2)

# 提取回歸係數
coefs <- coef(mod2)
beta3 <- coefs["exper"]          # exper 的係數
beta4 <- coefs["I(exper^2)"]     # exper^2 的係數
beta5 <- coefs["educ:exper"]     # educ*exper 的係數

# 從資料中提取 educ 和 exper
educ <- cps5_small$educ
exper <- cps5_small$exper

# 計算每個觀察值的邊際效應
marginal_effect_exper <- beta3 + 2 * beta4 * exper + beta5 * educ

# 繪製直方圖（使用基本 hist）
hist(marginal_effect_exper, 
     main = "Histogram of Marginal Effect of exper on log(wage)", 
     xlab = "Marginal Effect", 
     col = "lightgreen", 
     breaks = 50)

# （可選）使用 ggplot2 繪製更美觀的直方圖
df <- data.frame(marginal_effect_exper = marginal_effect_exper)
ggplot(df, aes(x = marginal_effect_exper)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Histogram of Marginal Effect of exper on log(wage)", 
       x = "Marginal Effect", y = "Frequency") +
  theme_minimal()


summary(marginal_effect_exper)
quantile(marginal_effect_exper,c(0.05,0.5,0.95))                    

#(f)
# 提取模型的係數
coefs <- coef(mod2)
beta0 <- coefs["(Intercept)"]
beta1 <- coefs["educ"]
beta2 <- coefs["I(educ^2)"]
beta3 <- coefs["exper"]
beta4 <- coefs["I(exper^2)"]
beta5 <- coefs["educ:exper"]

# 定義線性組合的係數向量（假設 beta6 是 beta0 的筆誤）
# 對應 mod2 的順序：(Intercept), educ, I(educ^2), exper, I(exper^2), educ:exper
combo <- c(152, 0, -1, -33, 10, 260)  # 152*beta0 + 0*beta1 + (-1)*beta2 + (-33)*beta3 + 10*beta4 + 260*beta5

# 提取變異數-共變異數矩陣
vcov_matrix <- vcov(mod2)

# 計算線性組合的變異數
var_combo <- t(combo) %*% vcov_matrix %*% combo

# 計算標準誤（變異數的平方根）
se_combo <- sqrt(var_combo)

# 計算線性組合的值
value_combo <- 152 * beta0 + 0 * beta1 - beta2 - 33 * beta3 + 10 * beta4 + 260 * beta5

# 顯示結果
cat("Linear Combination Value:", value_combo, "\n")
cat("Standard Error of Linear Combination:", se_combo, "\n")


#(j)
# 提取係數
coefs <- coef(mod2)
beta3 <- coefs["exper"]
beta4 <- coefs["I(exper^2)"]
beta5 <- coefs["educ:exper"]

# 假設 Jill 的 educ 和當前經驗
educ_jill <- 16  # 替換為實際值
current_exper <- 11  # 替換為實際值

# 計算臨界點
exper_critical <- -(beta3 + beta5 * educ_jill) / (2 * beta4)

# 提取變異數-共變異數矩陣
vcov_matrix <- vcov(mod2)
vcov_sub <- vcov_matrix[c("exper", "I(exper^2)", "educ:exper"), 
                        c("exper", "I(exper^2)", "educ:exper")]

# 計算梯度
gradient <- c(
  -1 / (2 * beta4),
  (beta3 + beta5 * educ_jill) / (2 * beta4^2),
  -educ_jill / (2 * beta4)
)

# 計算變異數和標準誤
var_exper_critical <- t(gradient) %*% vcov_sub %*% gradient
se_exper_critical <- sqrt(var_exper_critical)

# 計算信賴區間
ci_lower <- exper_critical - 1.96 * se_exper_critical
ci_upper <- exper_critical + 1.96 * se_exper_critical

# 計算剩餘時間
time_remaining <- exper_critical - current_exper
ci_lower_remaining <- ci_lower - current_exper
ci_upper_remaining <- ci_upper - current_exper

# 顯示結果
cat("Time until marginal effect becomes negative:", time_remaining, "years\n")
cat("95% CI for time remaining:", ci_lower_remaining, "to", ci_upper_remaining, "years\n")
