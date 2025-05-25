library(POE5Rdata)
data("liquor5")
library(dplyr)

liquor5 <- liquor5 %>%
           arrange(hh, year) %>%           
           group_by(hh) %>%               
           mutate(liquord = liquor - lag(liquor),  
                  incomed = income - lag(income)) %>%  
           ungroup() %>%
           filter(!is.na(liquord) & !is.na(incomed))  

first_differenced <- lm(liquord ~ incomed -1, data = liquor5)

fe_sum = summary(first_differenced)

conf_interval <- confint(first_differenced, "incomed", level = 0.95)
print(conf_interval)

#b
library(plm)
re_mod <- plm(liquor ~ income, data = liquor5, model = "random")
re_sum = summary(re_mod)
re_coeff = re_sum$coefficients[2, 1]
re_se = re_sum$coefficients[2, 2]
re_ci <- re_coeff + c(-1, 1) * 1.96 * re_se
cat("b. 95% re_CI：", round(re_ci, 4),"with se", re_se)
cat("a. 95% re_CI：", conf_interval, "with se", fe_sum$coefficients[2])

#c
lm_test <- plmtest(liquor ~ income, data = liquor5, effect = "individual")
print(lm_test)
#p-value = 0.01319
#H0: there's no indivisual difference
#reject H0

#d
data("liquor5")
liquor5$incomem <- ave(liquor5$income, liquor5$hh)
re_mod2 <- plm(liquor ~ income + incomem, data = liquor5, model = "random")
summary(re_mod2)
#p-value 0.76700  
#H0: no endogeneity
#non reject H0, no significant evidence that is endogenous
#library(lmtest)  
#coeftest(re_mod2, vcov. = vcovHC(re_mod2, type = "HC0"))
