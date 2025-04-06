# 載入資料
data <- read.csv("cps5_small.csv")

# 建立變數
data$educ2 <- data$educ^2
data$exper2 <- data$exper^2
data$educ_exper <- data$educ * data$exper
data$ln_wage <- log(data$wage)

# 建立迴歸模型
model <- lm(ln_wage ~ educ + educ2 + exper + exper2 + educ_exper, data = data)

# 顯示摘要結果
summary(model)

#c
# 建立所需變數
data$educ2 <- data$educ^2
data$exper2 <- data$exper^2
data$educ_exper <- data$educ * data$exper
data$ln_wage <- log(data$wage)

# 建立模型
model <- lm(ln_wage ~ educ + educ2 + exper + exper2 + educ_exper, data = data)

# 抓出係數
b <- coef(model)

# 計算每筆資料的邊際效果 ∂E[ln(WAGE)]/∂EDUC
data$marginal_effect <- b["educ"] + 2 * b["educ2"] * data$educ + b["educ_exper"] * data$exper

# 繪製 histogram
hist(data$marginal_effect,
     main = "Histogram of Marginal Effects of EDUC on ln(WAGE)",
     xlab = "Marginal Effect",
     col = "skyblue",
     border = "black")

# 計算中位數、第 5 百分位數與第 95 百分位數
median_effect <- median(data$marginal_effect)
p5_effect <- quantile(data$marginal_effect, 0.05)
p95_effect <- quantile(data$marginal_effect, 0.95)

# 印出結果
cat("Median:", median_effect, "\n")
cat("5th Percentile:", p5_effect, "\n")
cat("95th Percentile:", p95_effect, "\n")

#f
# 檢定 Svetlana 和 David 的期望 log-wage 是否有顯著差異

# 設定對應變數（模型有：educ, educ2, exper, exper2, educ_exper）
X_david <- c(1, 17, 17^2, 8, 8^2, 17 * 8)
X_svet  <- c(1, 16, 16^2, 18, 18^2, 16 * 18)

# 建立線性組合向量：David - Svetlana
L <- X_david - X_svet

# 使用 model (lm 物件) 計算估計差異及其標準誤
est_diff <- sum(L * coef(model))                                # 差異估計值
se_diff <- sqrt(t(L) %*% vcov(model) %*% L)                     # 差異的標準誤

# 計算 t 統計量與 p 值（右尾檢定）
t_stat <- est_diff / se_diff
p_value <- pt(t_stat, df = df.residual(model), lower.tail = FALSE)

# 顯示結果
cat("Estimated Difference:", est_diff, "\n")
cat("Standard Error:", se_diff, "\n")
cat("t Statistic:", t_stat, "\n")
cat("p-value (one-sided):", p_value, "\n")

#i
data$educ2 <- data$educ^2
data$exper2 <- data$exper^2
data$educ_exper <- data$educ * data$exper
data$ln_wage <- log(data$wage)

model <- lm(ln_wage ~ educ + educ2 + exper + exper2 + educ_exper, data = data)

# 取出相關係數：beta4 為 exper, beta5 為 exper2, beta6 為 educ_exper
beta4 <- coef(model)["exper"]
beta5 <- coef(model)["exper2"]
beta6 <- coef(model)["educ_exper"]

# 計算 Jill 還需要增加的經驗 x，使得邊際效果 = 0
# Jill 現有 11 年經驗，16 年教育
x_hat <- - (beta4 + 16 * beta6) / (2 * beta5) - 11

# 計算 g 函數對 beta4, beta5, beta6 的梯度
grad <- c(
  -1/(2 * beta5),                          # ∂g/∂beta4
  (beta4 + 16 * beta6) / (2 * beta5^2),       # ∂g/∂beta5
  -8 / beta5                                # ∂g/∂beta6
)

# 取出這三個參數的共變異數矩陣
vcov_mat <- vcov(model)[c("exper", "exper2", "educ_exper"), 
                        c("exper", "exper2", "educ_exper")]

# 利用 Delta 方法計算 x_hat 的變異數及標準誤
var_x <- as.numeric(t(grad) %*% vcov_mat %*% grad)
se_x <- sqrt(var_x)

# 95% 信賴區間 (常用正態分布下的 1.96 倍)
CI_lower <- x_hat - 1.96 * se_x
CI_upper <- x_hat + 1.96 * se_x

# 顯示結果
cat("Jill 需要額外增加的經驗估計:", x_hat, "年\n")
cat("95% 信賴區間: (", CI_lower, ",", CI_upper, ")\n")