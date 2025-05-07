rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
mroz

# a

mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc>12, 1, 0)
mroz_clean <- subset(mroz, !is.na(wage) & wage > 0 & !is.na(exper) & !is.na(educ) & !is.na(mothercoll))
iv_model_2iv <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                        exper + I(exper^2) + mothercoll + fathercoll, data = mroz_clean)
resid_2iv <- resid(iv_model_2iv)
plot(mroz_clean$exper, resid_2iv)
--圖看起來有一直變異數的問題

#  b

mroz_clean$e2_iv <- resid_2iv^2
test_model <- lm(e2_iv ~ exper, data = mroz_clean)
r2 <- summary(test_model)$r.squared
n <- nrow(mroz_clean)
nr2 <- n * r2
p_value <- 1 - pchisq(nr2, df = 1)
cat("NR^2 =", nr2, "\nP-value =", p_value, "\n")
--因爲 p-value = 0.0056 < 0.01，所以拒絕虛無假設，有證據支持模型具有異質變異數

# c

coeftest(iv_model_2iv, vcov = vcovHC(iv_model_2iv, type = "HC1"))
coeftest(iv_model_2iv)
--robust 標準誤大於一般標準誤，代表模型存在異質變異數問題，需用 robust 來估計
se_robust <- sqrt(diag(vcovHC(iv_model_2iv, type = "HC1")))
coef_educ <- coef(iv_model_2iv)["educ"]
se_educ <- se_robust["educ"]
ㄍlower <- round(coef_educ - 1.96 * se_educ, 4)
upper <- round(coef_educ + 1.96 * se_educ, 4)
cat("95% CI for EDUC (robust): [", lower, ", ", upper, "]\n")
--區間估計為 [0.0213, 0.1544] ，沒有包含0，代表 educ 具有顯著影響

# d 

boot_fn <- function(data, indices) {
  d <- data[indices, ]  
  model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                   exper + I(exper^2) + mothercoll + fathercoll,
                 data = d)
  return(coef(model)["educ"])
}
set.seed(123) 
boot_result <- boot(data = mroz_clean, statistic = boot_fn, R = 200)
boot_se <- sd(boot_result$t)
summary(iv_model_2iv)$coefficients["educ", "Std. Error"]
boot.ci(boot_result, type = "perc")
--使用兩階度估計的標準誤是 0.0308，使用 bootstrap 法的標準誤是 0.0338, robust 標準誤為 0.034
--因爲 bootstrap 標準誤大於兩階段的，所以使用 bootstrap 的模型更穩定，
--又因爲 bootstrap & robust 標準誤兩者差距不大，代表模型是穩定的
--因爲使用 bootstrap 得出的區間估計為 [0.0149, 0.1562], 不包含0
--所以 educ 是顯著的變數











 


