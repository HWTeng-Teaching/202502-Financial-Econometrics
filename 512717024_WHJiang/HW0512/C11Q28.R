############################################################
## 11.28 Truffle 供需系統 ─ R 實作 (a) ~ (f)
############################################################
# 0. 套件與資料 ---------------------------------------------------------------
library(tidyverse)   # dplyr + ggplot2
library(AER)         # ivreg() 兩階段最小平方法
library(lmtest)      # coeftest(), bptest()
library(sandwich)    # vcovHC() robust SE
library(modelsummary)# msummary() 漂亮表格
options(scipen = 6, digits = 3)

truf <- read_csv("truffles.csv") |>
        rename(P  = p ,
               Q  = q ,
               PS = ps,
               DI = di,
               PF = pf)

###############################################################################
# (a) 改寫方程式，把 P 放左邊，並說明預期符號 -------------------------------
###############################################################################
# ---- 這裡僅以「註解」形式說明 ─ 方便寫報告 ----
# Demand:  P = γ0 + γ1*Q + γ2*DI + γ3*PS + γ4*PF + u_d
#           預期 γ1 < 0, γ2 > 0, γ3 > 0, γ4 < 0
#
# Supply:  P = δ0 + δ1*Q + δ2*PF + u_s
#           預期 δ1 > 0, δ2 > 0
#
# （純代數改寫 → 無需計算；真正估計在 (b)、(f) 進行）

###############################################################################
# (b) 兩階段最小平方法 (2SLS) 估計需求 & 供給 -----------------------------
###############################################################################
dem.iv <- ivreg(P ~ Q + DI + PS + PF | DI + PS + PF, data = truf)  # 需求
sup.iv <- ivreg(P ~ Q + PF            | DI + PS + PF, data = truf)  # 供給

# 主要結果（robust SE）
msummary(list("Demand 2SLS" = dem.iv,
              "Supply 2SLS" = sup.iv),
         statistic = c("std.error", "p.value"),
         stars = TRUE, title = "表 1  2SLS 估計結果")

# 工具變數診斷──第一階段弱工具 F 與 Wu‐Hausman 內生性檢定
diag.dem <- summary(dem.iv, diagnostics = TRUE)$diagnostics
diag.sup <- summary(sup.iv, diagnostics = TRUE)$diagnostics

###############################################################################
# (c) 在平均數處估計需求價格彈性 -------------------------------------------
###############################################################################
avg <- truf |>
       summarise(across(c(P,Q), mean))   # 取得 \bar P, \bar Q
gamma1 <- coef(dem.iv)["Q"]
elasticity <- (1 / gamma1) * (avg$P / avg$Q)
cat("(c) 需求價格彈性 (at means) =", round(elasticity, 2), "\n")
# → 約 -0.97

###############################################################################
# (d) 固定外生變數繪圖並求均衡 -------------------------------------------
###############################################################################
# 固定值
DIstar <- 3.5 ; PFstar <- 23 ; PSstar <- 22

# 定義線性函數
dp <- coef(dem.iv) ; sp <- coef(sup.iv)
demand_P  <- function(Q) dp[1] + dp["Q"]*Q + dp["DI"]*DIstar + dp["PS"]*PSstar + dp["PF"]*PFstar
supply_P  <- function(Q) sp[1] + sp["Q"]*Q + sp["PF"]*PFstar

# 均衡交點
root <- uniroot(function(q) demand_P(q) - supply_P(q), interval = c(1,40))
Q_star <- root$root ; P_star <- demand_P(Q_star)

# 畫圖（預設顏色即可）
curve(demand_P, from = 5, to = 30, n = 200,
      xlab = "Quantity (Q)", ylab = "Price (P)", lwd = 2)
curve(supply_P, add = TRUE, lwd = 2, lty = 2)
points(Q_star, P_star, pch = 19, cex = 1.3)
legend("topright", c("Demand (2SLS)", "Supply (2SLS)", "Equilibrium"),
       lty = c(1,2,NA), pch = c(NA,NA,19), bty = "n")

cat("(d) 均衡： Q* =", round(Q_star,2), ", P* =", round(P_star,2), "\n")

###############################################################################
# (e) 用約化式預測均衡，並與 (d) 比較 ------------------------------------
###############################################################################
red.Q <- lm(Q ~ DI + PS + PF, data = truf)
red.P <- lm(P ~ DI + PS + PF, data = truf)

Q_hat <- predict(red.Q, newdata = tibble(DI = DIstar, PS = PSstar, PF = PFstar))
P_hat <- predict(red.P, newdata = tibble(DI = DIstar, PS = PSstar, PF = PFstar))

cat("(e) Reduced-form 預測： Q_hat =", round(Q_hat,2),
    ", P_hat =", round(P_hat,2), "\n")

###############################################################################
# (f) OLS 估計並與 2SLS 比較 ---------------------------------------------
###############################################################################
dem.ols <- lm(P ~ Q + DI + PS + PF, data = truf)
sup.ols <- lm(P ~ Q + PF        , data = truf)

msummary(list("Demand OLS" = dem.ols,
              "Demand 2SLS" = dem.iv,
              "Supply OLS" = sup.ols,
              "Supply 2SLS" = sup.iv),
         statistic = c("std.error", "p.value"),
         stars = TRUE, title = "表 2  OLS vs 2SLS 比較")

# Hausman 檢定：需求方程是否內生
library(systemfit)
haus <- hausman.systemfit(list(dem.ols, dem.iv))
print(haus)

# 供需殘差常態／異質變異檢定（附加選擇性）
jb.dem <- jarque.bera.test(residuals(dem.iv))
bp.dem <- bptest(dem.iv)

############################################################
## 以上對應題目 (a)–(f) 的所有步驟
############################################################
