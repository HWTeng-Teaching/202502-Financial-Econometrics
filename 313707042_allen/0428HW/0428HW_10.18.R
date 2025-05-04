install.packages("AER")
library(POE5Rdata)
library(AER)
data(mroz)
?data
married_data = mroz[mroz$lfp==1, ]
#a
married_data$mothercoll <- ifelse(married_data$mothereduc > 12, 1, 0)
married_data$fathercoll <- ifelse(married_data$fathereduc > 12, 1, 0)
mother_percent <- mean(married_data$mothercoll) * 100
father_percent <- mean(married_data$fathercoll) * 100

cat("有大學教育的母親比例：", round(mother_percent, 2), "%\n")
cat("有大學教育的父親比例：", round(father_percent, 2), "%\n")
#b
cor_matrix <- cor(married_data[, c("educ", "mothercoll", "fathercoll")])
print(round(cor_matrix, 4))
#C
library(AER) 
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll + exper + I(exper^2), data = married_data)
summary(iv_model,level=0.95)
confint(iv_model, level = 0.95)
#d
summary(iv_model, diagnostics = TRUE)
#e
iv2_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothercoll+fathercoll + exper + I(exper^2), data = married_data)
summary(iv2_model)
confint(iv2_model, level = 0.95)
#f#g
summary(iv2_model, diagnostics = TRUE)