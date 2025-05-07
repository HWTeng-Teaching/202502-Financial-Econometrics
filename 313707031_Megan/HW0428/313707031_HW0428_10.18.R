library(POE5Rdata)
data("mroz")

install.packages(c("wooldridge","AER","car","lmtest","sandwich","boot"))
library(AER)
library(car)
library(lmtest)
library(sandwich)

#10.18(a)
married_data = mroz[mroz$lfp==1, ]
married_data$mothercoll <- ifelse(married_data$mothereduc > 12, 1, 0)
married_data$fathercoll <- ifelse(married_data$fathereduc > 12, 1, 0)
pct_mother <- mean(married_data$mothercoll) * 100
pct_father <- mean(married_data$fathercoll) * 100

cat("母親有大學教育者比例：", round(pct_mother, 2), "%\n")
cat("父親有大學教育者比例：", round(pct_father, 2), "%\n")

#10.18(b)
corr_matrix <- cor(married_data[, c("educ", "mothercoll", "fathercoll")])
print(round(corr_matrix, 4))

#10.18(c)
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll + exper + I(exper^2), data = married_data)
summary(iv_model,level=0.95)
confint(iv_model, level = 0.95)


#10.18(d)
summary(iv_model, diagnostics = TRUE)

#10.18(e)
iv2_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll+fathercoll + exper + I(exper^2), data = married_data)
summary(iv2_model)
confint(iv2_model, level = 0.95)

#10.18(f,g)
summary(iv2_model, diagnostics = TRUE)
