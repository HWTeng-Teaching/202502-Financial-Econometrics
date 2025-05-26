# 載入必要套件
library(dplyr)

# 1. 讀入資料
df <- read.csv("liquor5.csv")

# 2. 依 hh, year 排序，並做 first‐difference
df_diff <- df %>%
  arrange(hh, year) %>%
  group_by(hh) %>%
  mutate(
    LIQUORD  = liquor - lag(liquor),
    INCOMED  = income - lag(income)
  ) %>%
  ungroup() %>%
  filter(!is.na(LIQUORD) & !is.na(INCOMED))

# 3. OLS 無截距迴歸
model <- lm(LIQUORD ~ 0 + INCOMED, data = df_diff)

# 4. 檢視摘要
summary(model)

# 5. 95% 信賴區間
confint(model, level = 0.95)



#(b)
# 載入套件並讀入資料
library(plm)

# 假設 liquor5.csv 已放在工作目錄下
dat <- read.csv("liquor5.csv")

# 建立 panel data：個體索引為 hh，時間索引為 year
pdata <- pdata.frame(dat, index = c("hh", "year"))

# 隨機效果模型：LIQUOR_it = β1 + β2·INCOME_it + u_i + e_it
re_mod <- plm(liquor ~ income,
              data  = pdata,
              model = "random",
              effect= "individual")

# 檢視摘要
summary(re_mod)

# 擷取係數與標準誤，計算 95% 信賴區間
est <- coef(summary(re_mod))["income", "Estimate"]
se  <- coef(summary(re_mod))["income", "Std. Error"]
ci_lower <- est - qnorm(0.975) * se
ci_upper <- est + qnorm(0.975) * se

cat(sprintf("β₂（INCOME）估計值 = %.3f\n95%% CI = [%.3f, %.3f]\n",
            est, ci_lower, ci_upper))




#(c)
library(plm)

# 讀入資料並建立面板資料
dat   <- read.csv("liquor5.csv")
pdata <- pdata.frame(dat, index = c("hh", "year"))

# （1）先做 pooled OLS（相當於 H0：u_i = 0，只有共同截距）
pool_mod <- plm(liquor ~ income,
                data  = pdata,
                model = "pooling")

# （2）對 pooled OLS 結果做 BP-LM 檢定
#     type = "bp" 代表 Breusch–Pagan LM test for random effects
lm_test <- plmtest(pool_mod, type = "bp")

print(lm_test)


#(d)
library(plm)

# 1. 讀入原始資料
dat <- read.csv("liquor5.csv")

# 2. 計算每個體的 INCOME 三年平均
inc_mean <- aggregate(income ~ hh, data = dat, FUN = mean)
names(inc_mean)[2] <- "incomem"

# 3. 把平均值合併回原始資料 dat
dat2 <- merge(dat, inc_mean, by = "hh")

# 4. 建立 panel data（這次把 incomem 一起納進去）
pdata <- pdata.frame(dat2, index = c("hh", "year"))

# 5. 跑含 INCOME 及 INCOMEM 的隨機效果模型
re_d <- plm(liquor ~ income + incomem,
            data   = pdata,
            model  = "random",
            effect = "individual")

# 6. 檢視結果，特別看 incomem（γ）的 p-value
summary(re_d)


