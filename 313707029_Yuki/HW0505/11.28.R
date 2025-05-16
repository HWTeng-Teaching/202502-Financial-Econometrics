#── 0. 清空環境 ───────────────────────────────
rm(list = ls())

#── 1. 載入所需套件 ───────────────────────────
# 如尚未安裝，請先：install.packages(c("POE5Rdata","dplyr","plm","lmtest","AER","ggplot2"))
library(POE5Rdata)
library(dplyr)
library(plm)
library(lmtest)
library(AER)       # for ivreg()
library(ggplot2)   # for plotting

#── 2. 載入 truffles 資料並檢查欄位 ────────────
data("truffles")
glimpse(truffles)
# 變數應該長這樣： p, q, di, ps, pf

#── 計算樣本平均值 ────────────────────────────
means <- truffles %>% 
  summarise(across(c(p, q, di, ps, pf), mean, .names="{.col}_bar"))
print(means)

#── (b) 用 2SLS 分別估計供給與需求方程式 ────────
# 供給： q ~ p + pf | instruments = pf + ps + di
supply_2sls <- ivreg(
  q ~ p + pf 
  | pf + ps + di, 
  data = truffles
)
summary(supply_2sls, diagnostics = TRUE)

# 需求： q ~ p + di + ps | instruments = di + ps + pf
demand_2sls <- ivreg(
  q ~ p + di + ps 
  | di + ps + pf, 
  data = truffles
)
summary(demand_2sls, diagnostics = TRUE)


#── (c) 價格彈性 of demand at the means ────────
coef_d <- coef(demand_2sls)["p"]
p_bar <- means$p_bar
q_bar <- means$q_bar
elasticity_d <- coef_d * (p_bar / q_bar)
cat("Demand price elasticity at means =", round(elasticity_d, 3), "\n")


#── (d) 畫出供給（實線）與需求（虛線）曲線 ─────
# 給定 exogenous 值
di_ <- 3.5; ps_ <- 22; pf_ <- 23

# 建立 Q 序列
q_seq <- seq(min(truffles$q), max(truffles$q), length.out = 100)

# 反寫 P = f(Q, exog)
s_coefs <- coef(supply_2sls)
p_supply <- (q_seq - s_coefs["(Intercept)"] - s_coefs["pf"] * pf_) / s_coefs["p"]

d_coefs <- coef(demand_2sls)
p_demand <- (q_seq - d_coefs["(Intercept)"] - d_coefs["di"] * di_ - d_coefs["ps"] * ps_) / d_coefs["p"]

df_curve <- data.frame(q = q_seq, p_supply, p_demand)
ggplot(df_curve, aes(x = q)) +
  geom_line(aes(y = p_supply), size = 1) +
  geom_line(aes(y = p_demand), linetype = "dashed", size = 1) +
  labs(
    title = "Supply (solid) and Demand (dashed) Curves",
    x = "Quantity (q)",
    y = "Price (p)"
  )


#── (e) 求均衡 (structural vs. reduced-form) ─────────────────
# Structural equilibrium: q_s = q_d → solve for p* and q*
p_eq_struct <- as.numeric(
  ( s_coefs["(Intercept)"] - d_coefs["(Intercept)"] 
    + s_coefs["pf"]*pf_ 
    - d_coefs["di"]*di_ 
    - d_coefs["ps"]*ps_ ) 
  / (d_coefs["p"] - s_coefs["p"])
)
q_eq_struct <- as.numeric(
  s_coefs["(Intercept)"] + s_coefs["p"] * p_eq_struct + s_coefs["pf"] * pf_
)
cat("Structural eq:  p* =", round(p_eq_struct,3), 
    ", q* =", round(q_eq_struct,3), "\n")

# Reduced-form: p ~ di + ps + pf; q ~ di + ps + pf
red_p <- lm(p ~ di + ps + pf, data = truffles)
red_q <- lm(q ~ di + ps + pf, data = truffles)
p_eq_rf <- predict(red_p, newdata = data.frame(di=di_, ps=ps_, pf=pf_))
q_eq_rf <- predict(red_q, newdata = data.frame(di=di_, ps=ps_, pf=pf_))
cat("Reduced-form eq:  p* =", round(p_eq_rf,3), 
    ", q* =", round(q_eq_rf,3), "\n")


#── (f) 用 OLS 估計供給與需求，並和 2SLS 比較 ─────
supply_ols <- lm(q ~ p + pf,    data = truffles)
demand_ols <- lm(q ~ p + di + ps, data = truffles)
summary(supply_ols)
summary(demand_ols)
