# 載入必要套件，若尚未安裝請先 install.packages("car") 與 install.packages("msm")
library(car)   # 提供 linearHypothesis 檢定
library(msm)   # 提供 deltamethod 函數進行 delta method
# 若需要其他繪圖功能可考慮載入 ggplot2

# 1. 讀取資料 (假設檔案名稱為 "cps5_small.csv" 且位於工作目錄中)
data <- read.csv("cps5_small.csv")

# 2. 估計模型
# 模型公式： ln(WAGE) = β1 + β2 * EDUC + β3 * EDUC^2 + β4 * EXPER + β5 * EXPER^2 + β6 * (EDUC * EXPER) + e
model <- lm(log(WAGE) ~ EDUC + I(EDUC^2) + EXPER + I(EXPER^2) + I(EDUC*EXPER), data = data)
summary(model)  # 查看估計結果與各係數的 p 值

# (a) 係數顯著水準
# 透過 summary(model) 中的 p-value 判斷每個係數在何顯著水準下顯著不為 0：
pvals <- summary(model)$coefficients[,4]
print(pvals)

# 3. 提取係數估計值
coef_est <- coef(model)

# (b) 計算 EDUC 的邊際效果： dln(WAGE)/dEDUC = β2 + 2*β3*EDUC + β6*EXPER
data$marginal_EDUC <- coef_est["EDUC"] +
                      2 * coef_est["I(EDUC^2)"] * data$EDUC +
                      coef_est["I(EDUC * EXPER)"] * data$EXPER

# (c) 繪製 EDUC 邊際效果直方圖，並計算中位數、第 5 與第 95 百分位
hist(data$marginal_EDUC, main = "Histogram of Marginal Effect (EDUC)",
     xlab = "Marginal Effect on log(WAGE)")
quantiles_EDUC <- quantile(data$marginal_EDUC, probs = c(0.05, 0.5, 0.95))
print(quantiles_EDUC)

# (d) 計算 EXPER 的邊際效果： dln(WAGE)/dEXPER = β4 + 2*β5*EXPER + β6*EDUC
data$marginal_EXPER <- coef_est["EXPER"] +
                       2 * coef_est["I(EXPER^2)"] * data$EXPER +
                       coef_est["I(EDUC * EXPER)"] * data$EDUC

# (e) 繪製 EXPER 邊際效果直方圖，並計算中位數、第 5 與第 95 百分位
hist(data$marginal_EXPER, main = "Histogram of Marginal Effect (EXPER)",
     xlab = "Marginal Effect on log(WAGE)")
quantiles_EXPER <- quantile(data$marginal_EXPER, probs = c(0.05, 0.5, 0.95))
print(quantiles_EXPER)

# (f) 檢定 David 與 Svetlana 的對數工資差異
# David: EDUC = 17, EXPER = 8
# Svetlana: EDUC = 16, EXPER = 18
# 預期對數工資差異 Delta = μ_D - μ_S =
#   = β2*(17-16) + β3*(17^2-16^2) + β4*(8-18) + β5*(8^2-18^2) + β6*(17*8-16*18)

Delta_vec <- c(1, (17^2 - 16^2), (8 - 18), (8^2 - 18^2), (17*8 - 16*18))
# 注意：截距項 (β1) 會相互抵消，所以對應的權重設為 0
L <- c(0, Delta_vec)
Delta_hat <- sum(coef_est * c(0, Delta_vec))
cov_mat <- vcov(model)
var_Delta_hat <- t(c(0, Delta_vec)) %*% cov_mat %*% c(0, Delta_vec)
se_Delta_hat <- sqrt(var_Delta_hat)
t_stat <- Delta_hat / se_Delta_hat
# 以一尾檢定 (H0: Δ ≤ 0, H1: Δ > 0)
p_val <- 1 - pt(t_stat, df = model$df.residual)
cat("Estimated Delta:", Delta_hat, "\nStandard Error:", se_Delta_hat,
    "\nt-statistic:", t_stat, "\nOne-sided p-value:", p_val, "\n")

# (g) 經過 8 年後的比較：
# 新狀況：David 的 EXPER 由 8 年變為 16 年；Svetlana 的 EXPER 由 18 年變為 26 年。
# 新差異：Delta' = β2*1 + β3*(17^2-16^2) + β4*(16-26) + β5*(16^2-26^2) + β6*(17*16-16*26)
Delta_vec_new <- c(1, (17^2 - 16^2), (16 - 26), (16^2 - 26^2), (17*16 - 16*26))
L_new <- c(0, Delta_vec_new)
Delta_hat_new <- sum(coef_est * c(0, Delta_vec_new))
var_Delta_hat_new <- t(c(0, Delta_vec_new)) %*% cov_mat %*% c(0, Delta_vec_new)
se_Delta_hat_new <- sqrt(var_Delta_hat_new)
t_stat_new <- Delta_hat_new / se_Delta_hat_new
p_val_new <- 1 - pt(t_stat_new, df = model$df.residual)
cat("Estimated Delta_new:", Delta_hat_new, "\nStandard Error:", se_Delta_hat_new,
    "\nt-statistic:", t_stat_new, "\nOne-sided p-value:", p_val_new, "\n")

# (h) 檢定 Wendy 與 Jill 的經驗邊際效果是否相等
# Wendy: EDUC = 12, EXPER = 17  => ME_W = β4 + 2β5*17 + β6*12
# Jill:  EDUC = 16, EXPER = 11  => ME_J = β4 + 2β5*11 + β6*16
# 差異: ME_W - ME_J = 12β5 - 4β6 = 4(3β5 - β6)
# 檢定 H0: 3β5 - β6 = 0  vs.  H1: 3β5 - β6 ≠ 0
# 構造對應係數約束向量 (模型中係數順序為：(Intercept), EDUC, I(EDUC^2), EXPER, I(EXPER^2), I(EDUC*EXPER))
L_hyp <- c(0, 0, 0, 0, 3, -1)
linearHypothesis(model, L_hyp)

# (i) Jill 的經驗邊際效果何時轉為負值
# 對 Jill (EDUC = 16) 的 EXPER 邊際效果： ME(EXPER) = β4 + 2β5*EXPER + β6*16
# 令 ME(EXPER*) = 0，即： β4 + 2β5*EXPER* + 16β6 = 0
# 解得： EXPER* = - (β4 + 16β6) / (2β5)
beta4 <- coef_est["EXPER"]
beta5 <- coef_est["I(EXPER^2)"]
beta6 <- coef_est["I(EDUC * EXPER)"]
EXPER_star <- -(beta4 + 16*beta6) / (2*beta5)
cat("Jill's EXPER* (marginal effect becomes zero):", EXPER_star, "\n")
# Jill 目前有 11 年經驗，所需額外年數為：
additional_years <- EXPER_star - 11
cat("Additional years for Jill until negative marginal effect:", additional_years, "\n")

# 利用 delta method 求 EXPER* 的 95% 信賴區間
# 定義 g(β) = - (β4 + 16β6) / (2β5)
g <- function(b) { -(b[1] + 16*b[3]) / (2*b[2]) }
b_est <- c(beta4, beta5, beta6)
# 使用 msm 套件的 deltamethod：
var_g <- deltamethod(~ -(x1 + 16*x3)/(2*x2), b_est, cov_mat[c("EXPER", "I(EXPER^2)", "I(EDUC * EXPER)"), c("EXPER", "I(EXPER^2)", "I(EDUC * EXPER)")])
se_g <- sqrt(var_g)
z <- qnorm(0.975)
CI <- c(EXPER_star - z*se_g, EXPER_star + z*se_g)
cat("95% CI for EXPER*:", CI, "\n")
# 若要得到 Jill 還需額外多少年，則信賴區間需減去現有 11 年：
CI_additional <- CI - 11
cat("95% CI for additional years for Jill:", CI_additional, "\n")
