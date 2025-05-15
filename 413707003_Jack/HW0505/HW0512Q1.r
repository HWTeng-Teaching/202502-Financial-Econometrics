# 11.28
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"))

# DEMAND
# P_i = delta_1 + delta_2 * Q_i + delta_3 * PS_i + delta_4 * DI_i + u_di

# delta_2 : negative
# delta_3 : positive
# delta_4 : positive


# SUPPLY
# P_i = theta_1 + theta_2 * Q_i + theta_3 * PF_i + u_si

# delta_2 : positive
# delta_3 : positive


# b

library(AER)

demand_2sls <- ivreg(p ~ q + ps + di | di + pf + ps, data=truffles)
summary(demand_2sls)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -11.428     13.592  -0.841  0.40810    
# q             -2.671      1.175  -2.273  0.03154 *  
# ps             3.461      1.116   3.103  0.00458 ** 
# di            13.390      2.747   4.875 4.68e-05 ***


supply_2sls <- ivreg(p ~ q + pf | di + pf + ps, data=truffles)
summary(supply_2sls)

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -58.7982     5.8592  -10.04 1.32e-10 ***
# q             2.9367     0.2158   13.61 1.32e-13 ***
# pf            2.9585     0.1560   18.97  < 2e-16 ***


# Yes. The signs are correct
# Yes. The estimated coefficients are significantly different from zero


