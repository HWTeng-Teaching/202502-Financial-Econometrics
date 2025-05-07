library(POE5Rdata)
data("mroz", package = "POE5Rdata")

names(mroz)

#a.
table(mroz$lfp)
working_married <- subset(mroz, lfp == 1)
nrow(working_married)  # 應該是 428

working_married$mothercoll <- ifelse(working_married$mothereduc > 12, 1, 0)
working_married$fathercoll <- ifelse(working_married$fathereduc > 12, 1, 0)

mother_pct <- mean(working_married$mothercoll, na.rm = TRUE) * 100
father_pct <- mean(working_married$fathercoll, na.rm = TRUE) * 100

cat(sprintf("母親有大學學歷的比例：%.2f%%\n", mother_pct))
cat(sprintf("父親有大學學歷的比例：%.2f%%\n", father_pct))

#a.ans

#母親有大學學歷的比例：12.15%
#父親有大學學歷的比例：11.68%

#b.

cor_matrix <- cor(working_married[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")

print(cor_matrix)

#b.ans

#The correlation between EDUC and MOTHERCOLL is 0.3595. 
#The correlation between EDUC and FATHERCOLL is 0.3985. 
#parent with high educ will have higher expectation to their children

#c.

install.packages("AER")
library(AER)

working_married$logwage <- log(working_married$wage)

iv_mother <- ivreg(logwage ~ educ + exper + I(exper^2) | mothercoll + exper + I(exper^2), data = working_married)

summary(iv_mother)

confint(iv_mother)

#c.ans
#educ        -0.001219763  1.532557e-01
#log(wage)=−0.1328+0.0760⋅educ+0.0433⋅exper−0.00087*exper2


#d.

first_stage <- lm(educ ~ mothercoll + exper + I(exper^2), data = working_married)
summary(first_stage)

car::linearHypothesis(first_stage, "mothercoll = 0")

#d.ans
#f-test=63.99 reject null hypothesis,mothercoll is a high instrument.

#e.

iv_both <- ivreg(logwage ~ educ + exper + I(exper^2) | mothercoll + fathercoll + exper + I(exper^2), data = working_married)
summary(iv_both)
confint(iv_both)

#e.ans
#log(wage)=−0.2791+0.08785⋅educ+0.04268⋅exper−0.00085⋅exper 2
#educ         0.02751845  1.481769e-01 narrower

#f.

first_stage_both <- lm(educ ~ mothercoll + fathercoll + exper + I(exper^2), data = working_married)
summary(first_stage_both)
car::linearHypothesis(first_stage_both, c("mothercoll = 0", "fathercoll = 0"))

#f.ans
#f-test=56.963 reject null hypothesis,the joint is adequately strong.

#g.
working_married$exper2 <- working_married$exper^2
library(ivmodel)

ivmod <- ivmodel(
  Y = working_married$logwage,
  D = working_married$educ,
  Z = working_married[, c("mothercoll", "fathercoll")],
  X = working_married[, c("exper", "exper2")]
)
summary(ivmod, diagnostics = TRUE)

#The Sargan test, is 0.237585< critical value chi-square=3.841,not reject h0,the instrument is vailed.



