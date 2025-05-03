library(POE5Rdata)
data("mroz")
?mroz

married_data = mroz[mroz$lfp==1, ]
#a 
#create MOTHERCOLL if MOTHEREDUC > 12, zero otherwise.
#FATHERCOLL equals one if FATHEREDUC > 12 and zero otherwise.
married_data$mothercoll <- ifelse(married_data$mothereduc > 12, 1, 0)
married_data$fathercoll <- ifelse(married_data$fathereduc > 12, 1, 0)

mother_college_pct <- mean(married_data$mothercoll) * 100
father_college_pct <- mean(married_data$fathercoll) * 100

cat("Percentage of wife's mothers with some college:", round(mother_college_pct, 2), "%\n")
cat("Percentage of wife's fathers with some college:", round(father_college_pct, 2), "%\n")  
#b
cor(married_data$educ, married_data$mothercoll, use = "complete.obs")#0.3594
cor(married_data$educ, married_data$fathercoll, use = "complete.obs")#0.3984
cor(married_data$mothercoll, married_data$fathercoll, use = "complete.obs")#0.3545
#相關係數越大，表示這些工具變數與教育變數 educ 越有關聯
#c
library(AER)
#install.packages("AER")
mroz.iv = ivreg(log(wage)~educ + exper + I(exper^2)| exper + I(exper^2) + mothercoll, data = married_data)
#confint(mroz.iv, level = 0.95)
vcoc_mroz = vcov(mroz.iv) 
mroz.iv$coefficients
df = mroz.iv$df.residual
tc = qt(0.975, df, lower.tail = TRUE)
educ_lb = mroz.iv$coefficients[2] - tc * (vcoc_mroz[2, 2]^0.5)
educ_ub = mroz.iv$coefficients[2] + tc * (vcoc_mroz[2, 2]^0.5)
cat("95% CI for educ coefficient: [", round(educ_lb, 5), ",", round(educ_ub, 5), "]\n")

#d
summary(mroz.iv, diagnostics = TRUE)
#f-statistic = 63.563
#strong

#e
mroz.iv2 = ivreg(log(wage)~educ + exper + I(exper^2)| exper + I(exper^2) + mothercoll + fathercoll, data = married_data)
vcoc_mroz2 = vcov(mroz.iv2) 
mroz.iv2$coefficients
df = mroz.iv2$df.residual
tc = qt(0.975, df, lower.tail = TRUE)
educ_lb = mroz.iv2$coefficients[2] - tc * (vcoc_mroz2[2, 2]^0.5)
educ_ub = mroz.iv2$coefficients[2] + tc * (vcoc_mroz2[2, 2]^0.5)
cat("95% CI for educ coefficient: [", round(educ_lb, 5), ",", round(educ_ub, 5), "]\n")
if(vcoc_mroz2[2, 2] > vcoc_mroz[2, 2]){
  print("wider than part c")
}else{
  print("narrower than part c")
}

#f
summary(mroz.iv2, diagnostics = TRUE)
#f-statistic 56.963
#strong

#g
summary(mroz.iv2, diagnostics = TRUE)
#Chi-square statistic 0.238, df = 1
#the extra instruments are valid






