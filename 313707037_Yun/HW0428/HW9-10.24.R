url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/mroz.dat"
download.file(url, destfile = "mroz.dat")
mroz <- read.table("mroz.dat", header = FALSE)
colnames(mroz) <- c("TAXABLEINC","FEDERALTAX","HSIBLINGS","HFATHEREDUC","HMOTHEREDUC","SIBLINGS","LFP","HOURS","KIDSL6","KIDS618","AGE","EDUC","WAGE","WAGE76","HHOURS","HAGE","HEDUC","HWAGE","FAMINC","MTR","MOTHEREDUC","FATHEREDUC","UNEMPLOYMENT","LARGECITY","EXPER"
)
head(mroz)  # 查看前幾行

# 載入必要的套件
install.packages("wooldridge")
library(wooldridge)
library(AER)         # for ivreg
library(lmtest)      # for bptest
library(sandwich)    # for robust/HC errors
library(boot)        # for bootstrapping
library(car)         # for linearHypothesis


# (a) 計算 IV/2SLS 殘差並畫出殘差與 EXPER 的圖
mroz_lfp <- subset(mroz, LFP == 1)
mroz_lfp$LWAGE <- log(mroz_lfp$WAGE)
mroz_lfp$EXPERSQ <- mroz_lfp$EXPER^2
library(AER)
iv_model_base <- ivreg(LWAGE ~ EXPER + EXPERSQ + EDUC |EXPER + EXPERSQ + MOTHEREDUC + FATHEREDUC, data = mroz_lfp)

# 取得殘差
resid_iv <- residuals(iv_model)
summary(iv_model_base)
# 繪製殘差與 EXPER 的散點圖
# 抓出模型實際用到的資料框
model_data <- model.frame(iv_model_base)

# 對應的 EXPER 值
exper_used <- model_data$EXPER

# 畫圖：EXPER vs 殘差（長度會對）
plot(exper_used, resid_iv,
     xlab = "EXPER", ylab = "IV Residuals", main = "Residuals vs EXPER")
abline(h = 0, col = "red")



# (b) 檢查異質變異：以 EXPER 為解釋變數回歸平方殘差
resid_sq <- resid_iv^2
bp_model <- lm(resid_sq ~ exper_used, data = mroz_lfp)
summary(bp_model)

# 計算 NR^2 統計量並檢定
n <- nrow(mroz_lfp)
R2 <- summary(bp_model)$r.squared
NR2 <- n * R2
pval <- 1 - pchisq(NR2, df = 1)
cat("NR^2 test statistic:", NR2, "\nP-value:", pval, "\n")

# (c) 使用 heteroskedasticity-robust 標準誤估計模型，並取得信mroz_lfp <- subset(mroz,LFP == 1)
mroz_lfp <- subset(mroz, LFP == 1)
mroz_lfp$LWAGE <- log(mroz_lfp$WAGE)
mroz_lfp$EXPERSQ <- mroz_lfp$EXPER^2
library(AER)
mroz_lfp$LWAGE <- log(mroz_lfp$LWAGE)
mroz_lfp$EXPERSQ <- mroz_lfp$EXPER^2
iv_model <- ivreg(LWAGE ~ EXPER + EXPERSQ + EDUC |EXPER + EXPERSQ + MOTHEREDUC + FATHEREDUC, data = mroz_lfp)
robust_se <- coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))
print(robust_se)

# 95% 信賴區間 for EDUC
educ_coef <- coef(iv_model)["educ"]
educ_se <- sqrt(vcovHC(iv_model, type = "HC1")["educ", "educ"])
ci_lower <- educ_coef - 1.96 * educ_se
ci_upper <- educ_coef + 1.96 * educ_se
cat("95% CI for EDUC (robust): [", ci_lower, ", ", ci_upper, "]\n")

#c
library(sandwich)
library(lmtest)
robust_se <- vcovHC(iv_model_base, type = "HC1")
coeftest(iv_model_base, vcov = robust_se)

b_educ <- coef(iv_model_base)["EDUC"]
se_educ <- sqrt(robust_se["EDUC", "EDUC"])
ci_lower <- b_educ - 1.96 * se_educ
ci_upper <- b_educ + 1.96 * se_educ

baseline_se <- coeftest(iv_model_base, vcov = vcov(iv_model_base))[, 2]
robust_se_vec <- sqrt(diag(robust_se))
estimates <- coef(iv_model_base)
se_comparison <- data.frame(
  Estimate     = round(estimates, 5),
  Baseline_SE  = round(baseline_se, 5),
  Robust_SE    = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No")
)

print(se_comparison)
cat('Conclusion:\nAs shown in the table, all robust standard errors are larger than their baseline counterparts. This is consistent with the presence of heteroskedasticity, which inflates the variability of the estimators when not properly accounted for.')
cat("95% Robust CI for EDUC: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")
# (d) Bootstrap 標準誤與信賴區間（B = 200）
mroz_lfp <- subset(mroz, LFP == 1)
mroz_lfp$LWAGE <- log(mroz_lfp$WAGE)
mroz_lfp$EXPERSQ <- mroz_lfp$EXPER^2
library(AER)
library(boot)
boot_iv <- function(data, indices) 
{d <- data[indices, ]
model <- ivreg(LWAGE ~ EXPER + EXPERSQ + EDUC |EXPER + EXPERSQ + MOTHEREDUC + FATHEREDUC, data = d)
  return(coef(model))
}
set.seed(123)
boot_result <- boot(data = mroz_lfp, statistic = boot_iv, R = 200)
boot_se <- apply(boot_result$t, 2, sd)
se_compare <- data.frame(
  Coef         = names(baseline_se),
  Baseline_SE  = round(baseline_se, 5),
  Robust_SE    = round(robust_se_vec, 5),
  Bootstrap_SE = round(boot_se, 5),
  Larger_than_Baseline_SE = ifelse(boot_se > baseline_se, "Yes", "No"),
  Larger_than_Robust_SE = ifelse(boot_se > robust_se_vec, "Yes", "No")
)
print(se_compare)
(boot_ci_educ <- boot.ci(boot_result, type = "norm", index = 4))



