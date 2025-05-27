install.packages("plm")
install.packages("POE5Rdata")
install.packages("lmtest")
install.packages("sandwich")

library(POE5Rdata)
library(plm)
library(lmtest)
library(sandwich)

data("liquor5")

# 15.17 (b)
pdata = pdata.frame(liquor5, index = c("hh", "year"))
randomModel = plm(liquor ~ income, data = pdata, model = "random")
summary(randomModel)

robustSE = sqrt(vcovHC(randomModel, type = "HC0")["income", "income"])
coefEst = coef(randomModel)["income"]
confInt = coefEst + c(-1, 1) * 1.96 * robustSE
cat("95% Confidence Interval:", round(confInt, 4), "\n")

# 15.17 (c)
bpTest = plmtest(liquor ~ income, data = pdata, effect = "individual", type = "bp")
print(bpTest)

# 15.17 (d)
pdata$incomeMean = ave(pdata$income, pdata$hh)
randomModel2 = plm(liquor ~ income + incomeMean, data = pdata, model = "random")
summary(randomModel2)
coeftest(randomModel2, vcov. = vcovHC(randomModel2, type = "HC0"))

# 15.20 (d)
rm(list = ls())
library(POE5Rdata)
library(plm)

data("star")
star$student_id <- star$id
panel_data <- pdata.frame(star, index = c("schid", "student_id"))

### (D) 執行 RE 與 Pooling 模型，並進行個體效果檢定
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = panel_data, model = "random", random.method = "swar")
summary(re_model)$coefficients

pool_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                  data = panel_data, model = "pooling")
plmtest(pool_model, effect = "individual")

### (E) FE 與 RE 估計結果的 t 檢定
fe_result <- summary(plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                         data = panel_data, model = "within"))$coefficients
re_result <- summary(re_model)$coefficients

for (j in seq_len(nrow(fe_result))) {
  se_sq_diff <- fe_result[j, 2]^2 - re_result[j + 1, 2]^2
  if (se_sq_diff > 0) {
    t_stat <- (fe_result[j, 1] - re_result[j + 1, 1]) / sqrt(se_sq_diff)
    cat(paste0("b", j + 1, " t-stat: ", round(t_stat, 3), "\n"))
  } else {
    cat(paste0("b", j + 1, " → invalid sqrt: diff = ", round(se_sq_diff, 4), "\n"))
  }
  if (j == nrow(fe_result)) {
    cat("Critical value (df=6): ±", round(qt(0.975, df = 6), 3), "\n")
  }
}

### (F) Mundlak approach：新增群體平均值後估計 RE 模型
cols <- c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")
for (var in cols) {
  panel_data[[paste0(var, "_mean")]] <- ave(panel_data[[var]], panel_data$schid)
}
panel_data <- na.omit(panel_data)

mundlak_model <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
    small_mean + aide_mean + tchexper_mean + boy_mean +
    white_asian_mean + freelunch_mean,
  data = panel_data,
  model = "random",
  random.method = "swar"
)
summary(mundlak_model)
