rm(list=ls()) # Caution: this clears the Environment
library(lmtest)
library(sandwich)
# 🔗 下載並載入 vacation 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(vacation)
##A###############
model <- lm(miles ~ income + age + kids, data =  vacation)
summary(model)
confint(model, 'kids', level = 0.95)
##B###############
res <- resid(model)
par(mfrow = c(1,2) )
## Residual vs Income
plot(vacation$income, res,
     xlab = 'Incomde @$1000',
     ylab = 'Residual',
     main = 'Residual vs Income @$1000')
abline(h = 0, lty = 2)
## Residual vs AGE
plot(vacation$age, res,
     xlab = 'age',
     ylab = 'Residual',
     main = 'Residual vs age')
abline(h = 0, lty = 2)
# 由圖觀察 Income 似乎有異質變異數的問題
##C########
#切分資料
vaca_sort <- vacation[order(vacation$income),]
data1 <- vaca_sort[1 : 90,]
data2 <- vaca_sort[111 : 200,]
#跑迴歸
m1 <- lm(miles ~ income + age + kids, data = data1)
m2 <- lm(miles ~ income + age + kids, data = data2)
#求變異數、殘差
sigsqr1 <- summary(data1$sigma^2)
sigsqr2 <- summary(data2$sigma^2)
# 假設已正確做過分組回歸 m1, m2
df1 <- m1$df.residual
df2 <- m2$df.residual

# 方法一：用 summary(model)$sigma^2
s1sq <- summary(m1)$sigma^2
s2sq <- summary(m2)$sigma^2

# 方法二：自己算
s1sq <- sum(resid(m1)^2) / df1
s2sq <- sum(resid(m2)^2) / df2

Fstat <- s2sq / s1sq           # 現在是單一純量
Fcrit <- qf(0.95, df2, df1)

cat("F =", round(Fstat,3),
    "  crit =", round(Fcrit,3), "\n")
##D#####
model <- lm(miles ~ income + age + kids, data = vacation)
# 1. 用 HC1 估 robust covariance matrix
vcov_hc1 <- vcovHC(model, type = "HC1")

# 2. 用 coeftest 顯示係數估計與 robust SE
coeftest(model, vcov.=vcov_hc1)

# 3. 取出 kids 係數的 95% robust CI
ci_robust <- confint(model, parm="kids", vcov.=vcov_hc1, level=0.95)
ci_robust
confint(model, 'kids', level = 0.95)
##E########
# （1）傳統 GLS（WLS）估計
gls_model <- lm(miles ~ income + age + kids,
                data = vacation,
                weights = 1/income^2)

# 取出「kids」95% CI
confint(gls_model, "kids", level = 0.95)

# （2）robust GLS 標準誤
library(lmtest)
library(sandwich)
# 用 HC1 估 robust covariance
vcov_gls_hc1 <- vcovHC(gls_model, type = "HC1")

# 顯示 robust 檢定結果
coeftest(gls_model, vcov.=vcov_gls_hc1)

# 取出「kids」係數的 robust 95% CI
# （confint.lm 內建不支援 vcov.，可手動算或用下面這段）
beta  <- coef(gls_model)["kids"]
se_r  <- sqrt(vcov_gls_hc1["kids","kids"])
alpha <- 0.05
ci_robust_gls <- beta + qt(c(alpha/2,1-alpha/2), df=gls_model$df.residual)*se_r
ci_robust_gls





