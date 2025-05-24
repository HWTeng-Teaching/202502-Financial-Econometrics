url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/truffles.dat"
download.file(url, destfile = "truffles.dat")
truffles <- read.table("truffles.dat", header = FALSE)
colnames(truffles) <- c("P","Q","PS","DI","PF")
head(truffles)  # 查看前幾行
# 載入必要套件
install.packages("systemfit")
library(systemfit)

#(b)
# 設定結構型方程式
demand.eq <- P ~ Q + PS + DI
supply.eq <- P ~ Q + PF

# 建立方程式清單
eqSystem <- list(demand = demand.eq, supply = supply.eq)

# 指定所有外生變數作為工具變數
instruments <- ~ PS + DI + PF

# 進行 2SLS 估計
fit.2sls <- systemfit(eqSystem, method = "2SLS", inst = instruments, data = truffles)

# 檢視結果
summary(fit.2sls)


#(c)
# 取得需求方程中 P 的係數
alpha2 <- coef(fit.2sls$eq[[1]])["P"]

# 計算均值
mean_P <- mean(truffles$P)
mean_Q <- mean(truffles$Q)

# 計算價格彈性
price_elasticity <- as.numeric(alpha2 * mean_P / mean_Q)
price_elasticity

#(d)
# 假設你已經有以下估計係數（請用你自己回歸的結果替換）
gamma1 <- -11.42841     # Demand 截距
gamma2 <- -2.67052    # Demand Q 係數
gamma3 <- 3.46108     # Demand PS 係數
gamma4 <- 13.38992     # Demand DI 係數

delta1 <- -58.798223       # Supply 截距
delta2 <- 2.936711     # Supply Q 係數
delta3 <- 2.958486    # Supply PF 係數

# 指定外生變數
PS_star <- 22
DI_star <- 3.5
PF_star <- 23

# 建立 Q 的範圍
Q <- seq(0, 50, length.out = 200)

# 計算需求與供給曲線上的價格
P_demand <- gamma1 + gamma2*Q + gamma3*PS_star + gamma4*DI_star
P_supply <- delta1 + delta2*Q + delta3*PF_star

# 畫圖
plot(Q, P_demand, type = "l", col = "blue", lwd = 2,
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Supply and Demand Curves")
lines(Q, P_supply, col = "orange", lwd = 2)
legend("topright", legend = c("Demand Curve", "Supply Curve"),
       col = c("blue", "orange"), lwd = 2)
grid()


#(e)
# 以 d 小題為例，假設已知參數如下
# Demand: P = a1 + a2*Q
# Supply: P = b1 + b2*Q

a1 <- -11.42841+3.46108*22+13.38992*3.5    # Demand 截距
a2 <- -2.67052  # Demand Q 係數
b1 <- -58.798223+2.958486*23    # Supply 截距
b2 <- 2.936711   # Supply Q 係數

# 假設你的資料框叫 truffles
# Q: 數量, P: 價格, PS: 替代品價格, DI: 收入, PF: 要素價格

# Reduced form for Q
rf_Q <- lm(Q ~ PS + DI + PF, data = truffles)
summary(rf_Q)

# Reduced form for P
rf_P <- lm(P ~ PS + DI + PF, data = truffles)
summary(rf_P)
# 取得係數
coef_Q <- coef(rf_Q)
coef_P <- coef(rf_P)

# 指定外生變數
PS_star <- 22
DI_star <- 3.5
PF_star <- 23

# 計算預測值
Q_hat <- coef_Q[1] + coef_Q["PS"]*PS_star + coef_Q["DI"]*DI_star + coef_Q["PF"]*PF_star
P_hat <- coef_P[1] + coef_P["PS"]*PS_star + coef_P["DI"]*DI_star + coef_P["PF"]*PF_star

Q_hat  # reduced form 預測的 Q
P_hat  # reduced form 預測的 P


# 聯立解交點
# a1 + a2*Q = b1 + b2*Q
# (a1 - b1) = (b2 - a2)*Q
Q_star <- (a1 - b1) / (b2 - a2)
P_star <- a1 + a2 * Q_star

Q_star  # 均衡數量
P_star  # 均衡價格

#(f)
# 需求方程 OLS 估計
demand_ols <- lm(P ~ Q + PS + DI, data = truffles)
summary(demand_ols)

# 供給方程 OLS 估計
supply_ols <- lm(P ~ Q + PF, data = truffles)
summary(supply_ols)

# 假設已經有 summary 結果
ols_demand_coef <- c(-13.6195, 0.1512, 1.3607, 12.3582)
sls_demand_coef <- c(-11.4284, -2.6705, 3.4611, 13.3899)
ols_supply_coef <- c(-52.8763, 2.6613, 2.9217)
sls_supply_coef <- c(-58.7982, 2.9367, 2.9585)

demand_df <- data.frame(
  Method = c("OLS", "2SLS"),
  Intercept = c(ols_demand_coef[1], sls_demand_coef[1]),
  Q = c(ols_demand_coef[2], sls_demand_coef[2]),
  PS = c(ols_demand_coef[3], sls_demand_coef[3]),
  DI = c(ols_demand_coef[4], sls_demand_coef[4])
)

supply_df <- data.frame(
  Method = c("OLS", "2SLS"),
  Intercept = c(ols_supply_coef[1], sls_supply_coef[1]),
  Q = c(ols_supply_coef[2], sls_supply_coef[2]),
  PF = c(ols_supply_coef[3], sls_supply_coef[3])
)

print(demand_df)
print(supply_df)
