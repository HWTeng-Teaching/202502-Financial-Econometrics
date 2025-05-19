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

summary(first_differenced)

conf_interval <- confint(first_differenced, "incomed", level = 0.95)
print(conf_interval)
