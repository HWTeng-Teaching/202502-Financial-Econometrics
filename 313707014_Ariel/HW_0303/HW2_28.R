#a.
library(ggplot2)  
library(POE5Rdata)
library(dplyr)
library(psych)

data ("cps5_small")
summary(cps5_small)
head(cps5_small)


wage_eduction <- cps5_small %>%
  select(educ,wage)

wage_eduction$educ <- as.numeric(wage_eduction$educ)  # 轉換整數欄位為數值
head (wage_eduction)  

hist(wage_eduction$educ,breaks =50, col='grey')
hist(wage_eduction$wage,breaks =50, col='grey')

describe(wage_eduction$educ)
summary(wage_eduction$educ)

describe(wage_eduction$wage)
summary(wage_eduction$wage)


#b.

mod <- lm(wage ~ educ, data = wage_eduction) #創建回歸
summary(mod)
b1 <- coef(mod)[[1]]
b2 <- coef(mod)[[2]]


#c.

wage_eduction$residuals <- residuals(mod)
sum (residuals(mod))


ggplot(wage_eduction, aes(x = educ, y = residuals )) +
  geom_point(color = "blue", size = 2) +  # 設定點的顏色與大小
  labs(title = "educ - residuals", x="educ", y="residuals")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)  # y=0 的虛線


#d.
data_black <- cps5_small %>%
  filter (black == 1)

data_white <- cps5_small %>%
  filter (black == 0)

data_male <- cps5_small %>%
  filter (female == 0)

data_female <- cps5_small %>%
  filter (female == 1)

run_regression  <- function(data) {
  model <- lm(wage ~ educ,data=data)  # 執行線性回歸
  return(summary(model))  # 回傳回歸結果
}


models <- list(
  "blacks" = run_regression(data_black),
  "whites" = run_regression(data_white),
  "male" = run_regression(data_male),
  "female" = run_regression(data_female)
 
)
models


#e. wage == 12

square_model <-  lm(wage~I(educ^2), data=cps5_small)
summary(square_model)
b1 <- coef(square_model)[[1]]
b2 <- coef(square_model)[[2]]
b2

educ_12 = c(12) 
wage_12 = b1+b2*educ_12^2 


margin_effect_12 <- 2*b2*educ_12 # 斜率
margin_effect_12



#wage == 16
educ_16 = c(16) 
wage_16 = b1+b2*educ_16^2 

margin_effect_16 <- 2*b2*educ_16 # 斜率
margin_effect_16



# f.



ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "grey", size = 2) +  # 設定點的顏色與大小
  labs(title = "educ-wage", x="educ", y="wage") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),color = "red", se = FALSE)







