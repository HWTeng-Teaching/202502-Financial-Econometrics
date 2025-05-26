# 載入套件
library(POE5Rdata)
library(ggplot2)
library(tidyverse)
library(AER)
library(systemfit)
library(broom)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(car)

# --- 15.17(b) ---
panel_liquor <- pdata.frame(liquor_data, index = c("hh", "year"))

liquor_re_model <- plm(liquor ~ income, data = panel_liquor, model = "random")
summary(liquor_re_model)

beta_val <- coef(liquor_re_model)["income"]
std_err  <- sqrt(vcov(liquor_re_model)["income", "income"])
ci_low   <- beta_val - qnorm(0.975) * std_err
ci_high  <- beta_val + qnorm(0.975) * std_err
c(`2.5 %` = ci_low, `97.5 %` = ci_high)

# --- 15.17(c) ---
plmtest(liquor ~ income, data = panel_liquor, type = "bp", effect = "individual")

# --- 15.17(d) ---
liquor_data <- liquor_data %>%
  group_by(hh) %>%
  mutate(income_avg = mean(income)) %>%
  ungroup()

panel_liquor2 <- pdata.frame(liquor_data, index = c("hh", "year"))

liquor_mundlak_model <- plm(liquor ~ income + income_avg, data = panel_liquor2, model = "random")
summary(liquor_mundlak_model)

mundlak_coef <- summary(liquor_mundlak_model)$coefficients
gamma_estimate <- mundlak_coef["income_avg", "Estimate"]
gamma_stderr   <- mundlak_coef["income_avg", "Std. Error"]
gamma_zvalue   <- mundlak_coef["income_avg", "z-value"]
gamma_pvalue   <- mundlak_coef["income_avg", "Pr(>|z|)"]
c(Estimate = gamma_estimate,
  `Std. Error` = gamma_stderr,
  `z value`    = gamma_zvalue,
  `p value`    = gamma_pvalue)

# --- 15.20(d) ---
panel_star <- pdata.frame(star, index = c("schid", "id"))

re_readscore_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                          data = panel_star, model = "random")
summary(re_readscore_model)

plmtest(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
        data = panel_star, type = "bp", effect = "individual")

# --- 15.20(e) ---
fe_readscore_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                          data = panel_star, model = "within")

hausman_result <- phtest(fe_readscore_model, re_readscore_model)
hausman_result

fe_coef_table <- coef(summary(fe_readscore_model))
re_coef_table <- coef(summary(re_readscore_model))

var_list <- c("small", "aide", "tchexper", "white_asian", "freelunch")

for (var in var_list) {
  coef_diff <- fe_coef_table[var, "Estimate"] - re_coef_table[var, "Estimate"]
  stderr_diff <- sqrt(fe_coef_table[var, "Std. Error"]^2 - re_coef_table[var, "Std. Error"]^2)
  t_value <- coef_diff / stderr_diff
  p_value <- 2 * (1 - pnorm(abs(t_value)))
  cat(sprintf("%-12s: t = %5.2f, p = %.3f\n", var, t_value, p_value))
}

# 額外：boy 變數單獨檢定
target_var <- "boy"
coef_diff <- fe_coef_table[target_var, "Estimate"] - re_coef_table[target_var, "Estimate"]
stderr_diff <- sqrt(fe_coef_table[target_var, "Std. Error"]^2 - re_coef_table[target_var, "Std. Error"]^2)
t_value <- coef_diff / stderr_diff
p_value <- 2 * (1 - pnorm(abs(t_value)))
cat(sprintf("%-12s: t = %5.2f, p = %.3f (boy)\n", target_var, t_value, p_value))

# --- 15.20(f) Mundlak 檢定版本 ---
# 確認時間變異的變數
timevar_candidates <- c("small", "aide", "tchexper", "freelunch")
timevar_list <- timevar_candidates[
  sapply(timevar_candidates, function(v) {
    all(tapply(star[[v]], star$schid, var, na.rm = TRUE) > 0, na.rm = TRUE)
  })
]

# 建立學校平均
school_means <- star %>%
  group_by(schid) %>%
  summarise(across(c(small, aide, tchexper, boy, white_asian, freelunch),
                   mean, .names = "{.col}_avg"))

# 合併資料與設為 panel
merged_data <- left_join(star, school_means, by = "schid") %>%
  pdata.frame(index = c("schid", "id"))

# 建立 OLS 模型含學校平均項
full_formula <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
  small_avg + aide_avg + tchexper_avg +
  boy_avg + white_asian_avg + freelunch_avg

ols_combined_model <- lm(full_formula, data = merged_data)
clustered_vcov <- vcovCL(ols_combined_model, cluster = ~ schid)

# 同時檢定所有學校平均項是否 = 0（Mundlak 檢定）
linearHypothesis(ols_combined_model,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = clustered_vcov)
