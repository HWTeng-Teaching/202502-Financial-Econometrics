# 5.31
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata"))

model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

# Call:
#   lm(formula = time ~ depart + reds + trains, data = commute5)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18.4389  -3.6774  -0.1188   4.5863  16.4986 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  20.8701     1.6758  12.454  < 2e-16 ***
#   depart        0.3681     0.0351  10.487  < 2e-16 ***
#   reds          1.5219     0.1850   8.225 1.15e-14 ***
#   trains        3.0237     0.6340   4.769 3.18e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.299 on 245 degrees of freedom
# Multiple R-squared:  0.5346,	Adjusted R-squared:  0.5289 
# F-statistic: 93.79 on 3 and 245 DF,  p-value: < 2.2e-16


# beta_1(Intercept): When DEPART, REDS, and TRAINS are 0, the expected travel time is 20.8701 minutes.
# beta_2(depart):For each additional minute of departure delay, the expected travel time increases by 0.3681 minutes.
# beta_3(reds):For each additional red light encountered, the expected travel time increases by 1.5219 minutes.
# beta_4(trains):For each additional train encountered, the expected travel time increases by 3.0237 minutes.


# b

confint(model, level = 0.95)
#                  2.5 %    97.5 %
# (Intercept) 17.5694018 24.170871
# depart       0.2989851  0.437265
# reds         1.1574748  1.886411
# trains       1.7748867  4.272505


# Yes.


# c

t_stat_reds <- (coef(model)["reds"] - 2) / summary(model)$coefficients["reds", "Std. Error"]
qt(0.05, 245)
# -1.651097
t_stat_reds
# -2.583562 

# Since t-value < qt(0.05, 245), we reject the null hypothesis, indicating that the delay caused by a red light is less than 2 minutes.



