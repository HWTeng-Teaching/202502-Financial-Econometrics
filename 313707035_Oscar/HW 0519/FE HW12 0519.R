# 如果尚未安裝，先安裝
install.packages(c("plm", "lmtest"))

library(POE5Rdata)
library(plm)
library(lmtest)
library(dplyr)

data("liquor5")

#(b)
pdat <- pdata.frame(liquor5, index = c("hh", "year"))

re_mod <- plm(liquor ~ income, data = pdat, model = "random")
summary(re_mod)
beta   <- coef(re_mod)["income"]
se     <- sqrt(vcov(re_mod)["income","income"])
ci_low  <- beta - qnorm(0.975) * se
ci_high <- beta + qnorm(0.975) * se
c(`2.5 %` = ci_low, `97.5 %` = ci_high)

#15.17.c
plmtest(liquor ~ income, data = pdat,
        type = "bp",        # Breusch–Pagan
        effect = "individual")

#15.17.d
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()

pdat2 <- pdata.frame(liquor5, index = c("hh", "year"))

re_mundlak <- plm(liquor ~ income + INCOMEM,
                  data = pdat2,
                  model = "random")

summary(re_mundlak)

coef_table <- summary(re_mundlak)$coefficients
gamma_est   <- coef_table["INCOMEM","Estimate"]
gamma_se    <- coef_table["INCOMEM","Std. Error"]
gamma_zstat <- coef_table["INCOMEM", "z-value"]
gamma_pval  <- coef_table["INCOMEM", "Pr(>|z|)"]
c(Estimate = gamma_est,
  `Std. Error` = gamma_se,
  `z value`    = gamma_zstat,
  `p value`    = gamma_pval)

data("star")
# 設定為面板資料
star.pdata <- pdata.frame(star, index = c("schid", "id"))

# 隨機效果模型
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star.pdata, model = "random", effect = "individual")
summary(re_model)

# LM檢定隨機效果
plmtest(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
        data = star.pdata, effect = "individual", type = "bp")

# 固定效果模型
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star.pdata, model = "within", effect = "individual")

# 抓係數與標準誤
coef_fe <- summary(fe_model)$coefficients[, 1]
se_fe   <- summary(fe_model)$coefficients[, 2]

coef_re <- summary(re_model)$coefficients[, 1]
se_re   <- summary(re_model)$coefficients[, 2]

vars_to_test <- c("small", "aide", "tchexper", "white_asian", "freelunch")

# t檢定 (b_FE - b_RE) / sqrt(se_FE^2 + se_RE^2)
t_stat <- (coef_fe[vars_to_test] - coef_re[vars_to_test]) / sqrt(se_fe[vars_to_test]^2 + se_re[vars_to_test]^2)
p_val  <- 2 * (1 - pnorm(abs(t_stat)))

results <- data.frame(
  variable = vars_to_test,
  t_stat = t_stat,
  p_value = p_val
)
results


# 計算每個學校平均值
for (var in c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")) {
  mn <- paste0(var, "_m")
  star[[mn]] <- ave(star[[var]], star$schid, FUN = mean)
}

# Mundlak 隨機效果模型
mundlak_model <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
    small_m + aide_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
  data = star.pdata, model = "random", effect = "individual"
)
summary(mundlak_model)
