#8.16
# 313707014 陳紀蓁

library(POE5Rdata)
library(tidyverse)
library(sandwich)
library(lmtest)
library(car)
library(xtable)
library(knitr)
library(sandwich)

data ("vacation")
summary(vacation)
head(vacation)

#a. 估計參數
mod1 <- lm(miles ~ income+age+kids, data=vacation)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, 
      caption="The basic multiple regression model", 
      col.names=c("coefficient", "Std. Error", "t-value", "p-value"),
      align="c", digits=3)

s <- summary(mod1)
s

#估計kids的信賴區間
a <- confint(mod1, "kids", level = 0.95)


#b.畫出殘差對income 和 殘差對age
residual <- residuals(mod1)


plot(vacation$income, residual, main="Residuals vs INCOME", xlab="INCOME", ylab="Residuals")
abline(h = 0, col = "red")


plot(vacation$age, residual, main="Residuals vs AGE", xlab="AGE", ylab="Residuals")
abline(h = 0, col = "red")


# 將income排序

vac_sorted <- vacation %>% arrange(income)
vac_sorted

# 抓出前 90 與後 90 筆資料

first90 <- vac_sorted[1:90, ]
first90
last90 <- vac_sorted[(nrow(vac_sorted)-89):nrow(vac_sorted), ]
last90


# 模型
model1 <- lm(miles ~ income + age + kids , data = first90)
summary(model1)
model2 <- lm(miles ~ income + age + kids , data = last90)
summary(model2)

# Goldfeld–Quandt test（使用 car 套件）
gqtest(miles ~ income + age + kids, order.by = ~income, data = vacation)


#d robust檢定（異質變異穩健標準誤）

coeftest(mod1, vcov = vcovHC(mod1, type = "HC1"))

# 95% 信賴區間（以 robust SE 為基礎）

robust_vcov <- vcovHC(mod1, type = "HC1")

confint_robust <- coefci(mod1,'kids', vcov. = robust_vcov, level = 0.95)

confint_robust


#e GLS 假設 
# 取權重的倒數（INCOME^2）
vacation$weight <- 1 / (vacation$income^2)

# GLS 加權最小平方法
gls_model <- lm(miles ~ income + age + kids , data = vacation, weights = weight)

# 顯示估計
summary(gls_model)

# 傳統 GLS 的 95% CI（未做 robust）
gls <- confint(gls_model, "kids", level = 0.95)
gls

robust_vcov_gls <- vcovHC(gls_model, type = "HC1")

# 使用 robust 標準誤建構 95% 信賴區間
confint_robust_gls <- coefci(gls_model,'kids', vcov. = robust_vcov_gls, level = 0.95)
confint_robust_gls



#做出比較


methods <- c("OLS（傳統標準誤）",
             "OLS + Robust SE（HC1）",
             "GLS（傳統 SE）",
             "GLS + Robust SE（HC1）")

# 下界和上界資料整理
ci_lower <- c(a[1], confint_robust[1], gls[1],confint_robust_gls[1])
ci_upper <- c(a[2], confint_robust[2], gls[2],confint_robust_gls[2])
ci_width <- round(ci_upper - ci_lower, 2)

# 放進 data.frame
ci_table <- data.frame(
  方法 = methods,
  `CI 下界` = round(ci_lower, 2),
  `CI 上界` = round(ci_upper, 2),
  `CI 寬度` = ci_width
)

# 顯示
print(ci_table)



