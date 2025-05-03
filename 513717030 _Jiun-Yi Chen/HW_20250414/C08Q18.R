##8.18
# install.packages("remotes")
# remotes::install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
data("cps5")
head(cps5)

names(cps5)

# Create exper square variable
cps5$exper2 <- cps5$exper^2

# Sample: Male and female

male_data <- subset(cps5, female == 0)
female_data <- subset(cps5, female == 1)

# Model formula (excluding female, because it is run separately)
formula_gq <- log(wage) ~ educ + exper + exper2 + black + metro + south + midwest + west

#
model_male <- lm(formula_gq, data = male_data)
model_female <- lm(formula_gq, data = female_data)

# SSE 與自由度
sse_male <- sum(resid(model_male)^2)
sse_female <- sum(resid(model_female)^2)

n_male <- nrow(male_data)
n_female <- nrow(female_data)
k <- length(coef(model_male))

# GQ 檢定統計量
F_stat <- (sse_female / (n_female - k)) / (sse_male / (n_male - k))

# 臨界值（5% 雙尾）
crit_low <- qf(0.025, df1 = n_female - k, df2 = n_male - k)
crit_high <- qf(0.975, df1 = n_female - k, df2 = n_male - k)

# 顯示結果
F_stat
crit_low
crit_high

sse_male
sse_female


##################b.

#Step 1: 建立原始迴歸模型（含全部變數）

# 建立原始模型（含 FEMALE）
model_ols_full <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# 取得殘差平方
cps5$resid_sq <- resid(model_ols_full)^2

#Step 2: 建立 auxiliary regression（METRO, FEMALE, BLACK）

# 跑 R^2-based test
aux_model <- lm(resid_sq ~ metro + female + black, data = cps5)

# 計算 Test Statistic
R2 <- summary(aux_model)$r.squared
n <- nrow(cps5)
test_stat <- n * R2

# 查卡方臨界值（自由度 = 3）
crit_val <- qchisq(0.99, df = 3)

# 顯示結果
test_stat
crit_val



#####################
# Step 0: OLS 模型，資料是 cps5
model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# Step 1: 建立殘差平方的 log
cps5$resid_sq <- resid(model_ols)^2
cps5$log_resid_sq <- log(cps5$resid_sq)

# Step 2: 建立輔助模型，用 METRO 和 EXPER 預測 log(e^2)
aux_model <- lm(log_resid_sq ~ metro + exper, data = cps5)
summary(aux_model)
# 預期會得到 ~ -3.15 + 0.23 * metro + 0.0049 * exper

# Step 3: 預測變異函數 h_i = exp(預測值)
cps5$h <- exp(fitted(aux_model))

# Step 4: 用權重 1/h_i 建立 FGLS
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west,
                 data = cps5,
                 weights = 1 / cps5$h)

summary(model_fgls)

# Step 5: 提取 KIDS 以外你要比較的變數（如 female、black 等）或整體分析
confint(model_fgls)  # 查看所有信賴區間
