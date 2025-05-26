library(POE5Rdata)
data(star)

library(plm)
library(lme4)
library(dplyr)
library(wooldridge)

data("star", package = "wooldridge")

# (a)
model_ols <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
cat("\n(a) OLS 模型結果：\n")
print(summary(model_ols))

# (b) 
star$stid <- star$id
star$id <- NULL 
pdata <- pdata.frame(star, index = c("schid", "stid"))

fe_school <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata,
                 model = "within",
                 effect = "individual")
cat("\n固定效果模型結果：\n")
print(summary(fe_school))

# (c) 
f_test <- pFtest(fe_school, model_ols)
cat("\n(c) 固定效果顯著性檢定：\n")
print(f_test)

#d
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata,
                model = "random",
                effect = "individual")

cat("\n(d) 隨機效果模型結果：\n")
print(summary(re_model))

pooling_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                     data = pdata,
                     model = "pooling")

lm_test <- plmtest(pooling_model, type = "bp")

cat("\n(d) LM 檢定結果（檢查是否需要使用隨機效果）：\n")
print(lm_test)

#e
hausman_test <- phtest(fe_school, re_model)
cat("\n(e) Hausman 檢定結果（固定 vs 隨機效果）：\n")
print(hausman_test)

fe_tab <- coef(summary(fe_school))
re_tab <- coef(summary(re_model))

vars <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

cat("\n(e) 各變數固定與隨機效果估計差異的 t 檢定（公式 15.36）：\n")
cat("--------------------------------------------------------------\n")
for (v in vars) {
  beta_fe <- fe_tab[v, "Estimate"]
  se_fe   <- fe_tab[v, "Std. Error"]
  beta_re <- re_tab[v, "Estimate"]
  se_re   <- re_tab[v, "Std. Error"]
  
  diff    <- beta_fe - beta_re
  se_diff <- sqrt(se_fe^2 + se_re^2)  
  t_stat  <- diff / se_diff
  p_val   <- 2 * (1 - pnorm(abs(t_stat)))
  
  cat(sprintf("%-12s | FE = %7.3f | RE = %7.3f | t = %6.3f | p = %.4f %s\n",
              v, beta_fe, beta_re, t_stat, p_val,
              ifelse(p_val < 0.05, "<-- 顯著差異", "")))
}

#f
library(lmtest)
library(sandwich)
library(car)

sch_avg <- star %>%
  group_by(schid) %>%
  summarise(across(c(small, aide, tchexper, boy, white_asian, freelunch),
                   mean, .names = "{.col}_avg"))

star_m <- left_join(star, sch_avg, by = "schid") %>%
  pdata.frame(index = c("schid", "stid"))


mundlak_form <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
  small_avg + aide_avg + tchexper_avg + boy_avg + white_asian_avg + freelunch_avg


ols_m <- lm(mundlak_form, data = star_m)
vc_cl <- vcovCL(ols_m, cluster = ~ schid)

# 同時檢定所有平均變數是否為 0
mundlak_test <- linearHypothesis(ols_m,
                                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                                 vcov. = vc_cl)

cat("\n(f) Mundlak 檢定結果（學校平均變數是否顯著）：\n")
print(mundlak_test)
