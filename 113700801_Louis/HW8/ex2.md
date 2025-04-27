
# 1
β₂ positive : higher wage means higher number of working hours 

β₃ positive : better education means higher job opportunities/ job responsibilities 

β₄ positive or negative : young age -> more time to work, experience opportunities  (increase) ; older -> working less time to work (decrease)

β₅ negative : need to take care of the childrens so less time for work

β₆ negative : if the income from the husband is higher then no need to work too much

# 2
wage could be endogenous because it might be influanced by unobservable factors as for exemple the motivation, ability, job exeprience...and also could affect hours supply.

the error term may capture these unobservable factors and so make the estimators biased and inconsistent. 

# 3
Relevance : more experience increases wages and wages might flatten or decline after some point (expert²) -> relevant

Exogeneity : EXPER and EXPER² do not directly affect HOURS but have an impact on WAGE -> they should only shift labor supply by changing the wage rate.

Thus, EXPER and EXPER² satisfy the IV logic

# 4
We have one endogenous regressor (WAGE)
We have at least two instruments (EXPER and EXPER²)
so the model is over identified

# 5

First Stage Regression:
Regress WAGE on EXPER, EXPER², and any exogenous variables in the model (EDUC, AGE, KIDSL6, NWIFEINC).
Obtain the predicted values of WAGE 

Second Stage Regression:
Regress HOURS on the predicted WAGE and the original exogenous variables (EDUC, AGE, KIDSL6, NWIFEINC).
The coefficients from this second regression are your IV/2SLS estimates.



