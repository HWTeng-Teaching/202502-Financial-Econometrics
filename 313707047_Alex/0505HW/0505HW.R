##CH11.28
rm(list=ls()) 
library(POE5Rdata)
data(truffles)

# 安裝一次就好
install.packages("AER")     # 包含 ivreg 函數
install.packages("readr")   # 若你的檔案是 CSV 格式

# 載入套件
library(AER)
library(readr)

# 替換成你的實際檔名
truffles <- read_csv("truffles.csv")

# 檢查欄位名稱與資料摘要
names(truffles)
summary(truffles)

##b
# 需求方程：P ~ Q + PS + DI，工具變數是 PS, DI, PF
demand_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_iv)

# 供給方程：P ~ Q + PF，工具變數是 PF, PS, DI
supply_iv <- ivreg(p ~ q + pf | pf + ps + di, data = truffles)
summary(supply_iv)



##C
# 取得估計係數
delta_2 <- coef(demand_iv)["q"]

# 平均數計算
mean_p <- mean(truffles$p)
mean_q <- mean(truffles$q)

# 計算彈性
ed <- (1 / delta_2) * (mean_p / mean_q)
cat("需求的平均點價格彈性為：", round(ed, 3), "\n")




##D

demand_iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
supply_iv <- ivreg(p ~ q + pf | pf + ps + di, data = truffles)

library(ggplot2)

# 指定 exogenous variable values
ps_star <- 22
di_star <- 3.5
pf_star <- 23

# 提取係數
b_d <- coef(demand_iv)
b_s <- coef(supply_iv)

# 產生一組 Q 值（數量範圍）
q_vals <- seq(min(truffles$q), max(truffles$q), length.out = 100)

# 對應的 P 值（套用估計式）
p_demand <- b_d["(Intercept)"] + b_d["q"] * q_vals + b_d["ps"] * ps_star + b_d["di"] * di_star
p_supply <- b_s["(Intercept)"] + b_s["q"] * q_vals + b_s["pf"] * pf_star

# 整理成資料框
df_plot <- data.frame(q = q_vals, Demand = p_demand, Supply = p_supply)

# 繪圖
ggplot(df_plot, aes(x = q)) +
  geom_line(aes(y = Demand), color = "blue", size = 1.2) +
  geom_line(aes(y = Supply), color = "red", size = 1.2) +
  labs(title = "Truffle Market: Supply and Demand",
       x = "Quantity (q)", y = "Price (p)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))




##E
# 解供需交點 Q*, P*
# 將兩方程相等：Demand = Supply → 解 Q

a <- b_d["q"] - b_s["q"]
b <- (b_s["(Intercept)"] - b_d["(Intercept)"]) +
  b_s["pf"] * pf_star - b_d["ps"] * ps_star - b_d["di"] * di_star

q_eq <- b / a
p_eq <- b_d["(Intercept)"] + b_d["q"] * q_eq + b_d["ps"] * ps_star + b_d["di"] * di_star

cat("均衡數量 q* =", round(q_eq, 3), "\n")
cat("均衡價格 p* =", round(p_eq, 3), "\n")

# Reduced-form 係數
c <- -32.5124
b_ps <- 1.7081
b_di <- 7.6025
b_pf <- 1.3539

# 給定的外生變數
ps_star <- 22
di_star <- 3.5
pf_star <- 23
# Reduced-form Q 係數
c_q <- 7.8951
bq_ps <- 0.6564
bq_di <- 2.1672
bq_pf <- -0.5070

# 外生變數（如前）
ps_star <- 22
di_star <- 3.5
pf_star <- 23

# 預測價格
p_hat <- c + b_ps * ps_star + b_di * di_star + b_pf * pf_star
cat("Reduced-form 預測價格 P* =", round(p_hat, 3), "\n")


# 預測 Q
q_hat <- c_q + bq_ps * ps_star + bq_di * di_star + bq_pf * pf_star
cat("Reduced-form 預測數量 Q* =", round(q_hat, 3), "\n")





##F
# 安裝必要套件（如果尚未安裝）
install.packages("AER")
install.packages("broom")
install.packages("dplyr")

# 載入套件
library(AER)
library(broom)
library(dplyr)

# 載入資料
# 假設你已經讀入 truffles 資料
# truffles <- read.csv("truffles.csv")

# 1. 估計模型 --------------------------------------------------

# 需求方程：P ~ Q + PS + DI
demand_ols <- lm(p ~ q + ps + di, data = truffles)
demand_2sls <- ivreg(p ~ q +ps + di | ps + di + pf, data = truffles)

# 供給方程：P ~ Q + PF
supply_ols <- lm(p ~ q + pf, data = truffles)
supply_2sls <- ivreg(p ~ q + pf | pf + ps + di, data = truffles)

# 2. 擷取估計結果 --------------------------------------------------

# 用 broom::tidy 轉換回歸結果為表格
tidy_demand_ols <- tidy(demand_ols) %>% mutate(model = "Demand OLS")
tidy_demand_2sls <- tidy(demand_2sls) %>% mutate(model = "Demand 2SLS")
tidy_supply_ols <- tidy(supply_ols) %>% mutate(model = "Supply OLS")
tidy_supply_2sls <- tidy(supply_2sls) %>% mutate(model = "Supply 2SLS")

# 3. 合併所有模型結果 --------------------------------------------------

all_results <- bind_rows(tidy_demand_2sls, tidy_demand_ols,
                         tidy_supply_2sls, tidy_supply_ols) %>%
  select(model, term, estimate, std.error, statistic, p.value)

# 4. 顯示結果 --------------------------------------------------

# 整齊列印
print(all_results)

# 如需寫入 csv：
# write.csv(all_results, "regression_comparison_results.csv", row.names = FALSE)







##CH11.30
rm(list=ls()) 
library(POE5Rdata)
data(klein)


##A
# OLS 回歸：投資對消費與落後一期資本
klein_data <- klein
klein_data$klag <- lag(klein_data$k)

model_invest <- lm(i ~ p+plag + klag, data = klein)
summary(model_invest)


##B
profit_rf <- lm(p ~ g + w2 + tx + plag + klag + time + elag, data = klein)
summary(profit_rf)

restricted_model <- lm(p ~ plag + klag, data = klein)
anova(restricted_model, profit_rf)
cat('Conclusion:\nThe result (F = 1.93, p = 0.1566) indicates that the additional variables are not jointly statistically significant at the 5% level. Thus, conditional on lagged profit and capital stock, these exogenous variables do not significantly improve the explanatory power of the model.')
klein_clean$vt_hat <- residuals(profit_rf)
klein_clean$pt_hat <- fitted(profit_rf)
klein_clean[, c("p", "pt_hat", "vt_hat")]



##C
library(AER)
library(dplyr)

# 載入並轉成 data frame
data("klein")
df <- as.data.frame(klein)

# 只要刪掉有 NA 的列就好，因為 plag / klag 本來就有了
df <- df %>% na.omit()

# 建立 reduced-form for P
rf_p <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = df)

# 加入殘差與預測值
df$vhat <- resid(rf_p)
df$phat <- fitted(rf_p)

hausman_model <- lm(i ~ p + plag + klag + vhat, data = df)
summary(hausman_model)




##D
library(AER)
install.packages("broom")   # 如尚未安裝
library(broom)
library(dplyr)
library(AER)
ols_model <- lm(i ~ p + plag + klag, data = df)
summary(ols_model)
library(AER)
library(tidyr)

iv_model <- ivreg(i ~ p + plag + klag | g + w2 + tx + time + plag + klag + elag + cn, data = df)
summary(iv_model)

library(broom)
library(dplyr)

compare <- bind_rows(
  tidy(ols_model) %>% mutate(model = "OLS"),
  tidy(iv_model) %>% mutate(model = "2SLS")
)

print(compare)

iv_2sls <- ivreg(i ~ p + plag + klag | g + w2 + tx + plag + klag + time + elag, data = klein)
summary(iv_2sls)

##E
# 建立第二階段模型（用 phat 代替內生變數 P）
iv_stage2 <- lm(i ~ phat + plag + klag, data = df)

# 顯示估計結果
summary(iv_stage2)



##F
# Step 1: 取得第二階段殘差
df$e2hat <- resid(iv_stage2)

# Step 2: 用所有工具變數（5 個）來解釋殘差
# 工具變數為：g, w2, tx, time, elag（總共 L = 5 個）

sargan_test <- lm(e2hat ~ g + w2 + tx + time + elag, data = df)

# Step 3: 擷取 R^2 並計算檢定統計量 TR^2
r2 <- summary(sargan_test)$r.squared
N <- nrow(df)         # 樣本數
TR2 <- N * r2

# Step 4: 計算臨界值（自由度 = L - B = 4）
crit_val <- qchisq(0.95, df = 4)

# 輸出結果
TR2
crit_val

