## 1 

T-stat : 1.4515/2.2019=0,659 

S.E. : 2.7648/5.7103=0,484 

B_3 NK : 0.3695*(-3.9376)=-1,455 

R^2 : TSS = (6.39547^2)*(1200-1)=49 041,542 
      1-ESS/TSS = 1-46221.62/49 041,542=0,0575 

S.E. of regression : sqrt(46221.62/(1200-4))=6.217

## 2 

if total expenditure increases of 1 unit, the percentage of a household’s budget spent on alcohol will increase by 2.7648

if the number of children in the household increases of 1 unit, the percentage of a household’s budget spent on alcohol will decrease by -1,455

if the age of the household head increases of 1 unit, the percentage of a household’s budget spent on alcohol will decrease by -0.1503


## 3 

[-0.1503+1.96*0.0235 ; -0.1503-1.96*0.0235] = [-0,3 ; -0,196]

The value of B_4 has 95% chance of being in this range.

## 4

all coefficients are significant (except for the intercept, but we don't really care about this one). this means that all the predictors are usefull for the understanding of Y (percentage of a household’s budget spent on alcohol).

## 5

H_0 : B_3=-2
H_1 : B_3≠-2

t=(-1,455-(-2))/0.3695=1,475   ∣1.96∣ > ∣1,475∣ --> accept H_0, no significant evidence that the effect is different from −2 percentage points
