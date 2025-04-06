#5.31
rm(list = ls())

# 🔗 下載並載入 cex5_small 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(commute5)

#A####
model <- lm(time ~ depart + reds + trains, data =  commute5)
summary(model)
par(mfrow = c(2, 2))
plot(model)
#截距 20.87： 為理想情況下（即 Bill 6:30 準時出發且不遇到紅燈與火車）的預測通勤時間。
#DEPART (0.3681)： 每延遲 1 分鐘出發，通勤時間平均增加約 0.37 分鐘，反映出晚出發可能因為交通狀況較差而使通勤時間增加。
#REDS (1.5219)： 每多遇到一個紅燈，通勤時間平均增加約 1.52 分鐘，顯示紅燈的延誤效應。
#TRAINS (3.0237)： 每多等待一班火車，通勤時間平均增加約 3.02 分鐘，突顯火車等待時間對通勤時間的顯著影響。
#B####
conf_int <- confint(model, level = 0.95)
print(conf_int)
#由於所有係數的 95% 信賴區間都相對窄且均未包含0（顯著不為零），因此我們可以說各係數的估計相當精確。
#C####
# 已知的估計值與標準誤
beta_hat <- 1.5219
se <- 0.1850
df <- 245

# 虛無假設 H0: beta_reds = 2
beta0 <- 2

# 計算 t 統計量
t_stat <- (beta_hat - beta0) / se
t_stat

# 單尾檢定 p-value：因為我們檢定是否「小於」2，因此用 lower.tail = TRUE
p_value_one_tail <- pt(t_stat, df = df, lower.tail = TRUE)
p_value_one_tail
#p-value = 0.00516 < 0.05 reject H0

#D####
# 已知的估計值與標準誤
beta_hat <- 3.0237
se <- 0.6340
df <- 245
beta0 <- 3  # H0: β_trains = 3

# 計算 t 統計量
t_stat <- (beta_hat - beta0) / se
t_stat

# 取得雙尾檢定 p-value
# (因為對立假設是 "!="，所以要將單尾的 p-value 乘以 2)
p_value_two_tail <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
p_value_two_tail
# p-value = 0.97 > 0.05 don't reject H0

#E####
# 已知 depart 係數 (Estimate) 與標準誤 (Std. Error)
beta_depart <- 0.3681
se_depart   <- 0.0351

# 計算差距 (D_hat) 與其標準誤
D_hat  <- 30 * beta_depart
SE_D   <- 30 * se_depart

# 設定虛無假設下的差距值 (H0: D >= 10)
D0 <- 10

# 計算 t 統計量
t_stat <- (D_hat - D0) / SE_D
t_stat

# 左尾檢定 p-value：因為 H1: D < 10，故 lower.tail = TRUE
df <- 245
p_value <- pt(t_stat, df = df, lower.tail = TRUE)
p_value
# p-value = 0.83 > 0.05 don't reject H0

#F####
coefs <- coef(model)
vcmat <- vcov(model)

# 定義線性組合 L：trains - 3*reds
L <- c(0, 0, -3, 1)  

# 計算差異的點估計
D_hat <- sum(L * coefs)  # 相當於 L %*% coefs

# 計算差異的變異數
var_D <- t(L) %*% vcmat %*% L  # (1x4) * (4x4) * (4x1) => (1x1)

# 計算標準誤與 t 值
SE_D <- sqrt(var_D)
t_stat <- (D_hat - 0) / SE_D

# 單尾 p-value (左尾檢定)：因為 H1: D < 0
df <- model$df.residual
p_value <- pt(t_stat, df = df, lower.tail = TRUE)
p_value
# p-value = 0.034 < 0.05 reject H0
#G####
vcov(model)
# 模型係數（假設已知）
beta_trains <- 3.0237
beta_reds   <- 1.5219

# 計算檢定量 D 的點估計
D_hat <- beta_trains - 3 * beta_reds
D_hat  # -1.542

# 利用變異數–共變異數矩陣計算 D 的變異
# 從矩陣中取得相關值
var_reds   <- 0.0342390502
var_trains <- 0.4019709090
cov_reds_trains <- -0.0006481936

var_D <- 9 * var_reds + var_trains - 6 * cov_reds_trains
SE_D <- sqrt(var_D)
SE_D  # 約 0.845

# 計算 t 統計量
t_stat <- D_hat / SE_D
t_stat  # 約 -1.825

# 自由度 (df)，此處為 245
df <- 245

# 計算單尾 p-value (左尾)
p_value <- pt(t_stat, df = df, lower.tail = TRUE)
p_value

#H####
#當 Bill 必須準時時，我們希望將「遲到」作為預設狀態（H₀)
#H₀： Bill 的預期通勤時間 ≥ 45 分鐘
#H₁： Bill 的預期通勤時間 < 45 分鐘
#這樣的假設反轉有助於在決策時更加保守，只有在確定他能準時的強烈證據下，才會作出「準時」的結論。
