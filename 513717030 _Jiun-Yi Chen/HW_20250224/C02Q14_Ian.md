![圖片](https://github.com/user-attachments/assets/373cd42f-5040-4e76-a418-70492eaeadd8)
![圖片](https://github.com/user-attachments/assets/5bf60269-e8cc-4bf1-91b9-07d115350a8d)

## (a)
Suppose Wage =W , mean education(EDUC) = $\bar{x}$
\
When $\bar{W} =19.74$,  fit it in rural regression line function , we can find $\bar{x}$= (19.74+4.88) / 1.8 = 13.6778
![圖片](https://github.com/user-attachments/assets/19ce7560-e650-4ce4-ad60-8bb4b0d5ec06)
![圖片](https://github.com/user-attachments/assets/48a7689c-2639-4245-ac0e-96ac41e31ff6)

Elasticity = $\frac{\partial WAGE}{\partial EDUC}$ $\frac{EDUC}{WAGE}$ = $\beta_2  \frac{X}{W}$ = 1.80 * 13.6778/19.74 = 1.2472

## (b)
We are given the mean level of $EDUC$. Therefore,
$\hat{WAGE} = -10.76 + 2.46 EDUC$ and $\bar{X}$ = 13.68  then $\bar{W} = 22.8928$

$\hat{\epsilon}  = \frac{\partial W}{\partial X} \times \frac{X}{W} = \beta_2 \times \frac{X}{W}.$

using $\bar{X}, \bar{W}$  then $\hat{\epsilon} = \beta_2 \times \frac{\bar{X}}{\bar{W}}$ = 2.46 (13.68/22.8928) =1.4700

$\mathrm{Var} (\hat{\epsilon})
= \left(\frac{\partial E}{\partial \beta_2}\right)^2 
  \mathrm{Var}(\beta_2) 
= \left(\frac{\bar{X}}{\bar{W}}\right)^2 
  \mathrm{Var}(\beta_2)$ 

$\mathrm{se}(\hat{\epsilon}) 
= \sqrt{\mathrm{Var}(\hat{\epsilon})} 
= \left|\frac{\bar{X}}{\bar{W}}\right| 
  \,\mathrm{se}(\beta_2)$ =  (13.68/22.8928)*0.16 = 0.0956 


## (c)
*in urban area*\
$\hat{WAGE}$ where EDUC=12 = -10.76 +2.46 * 12 = 18.76\
$\hat{WAGE}$ where EDUC=16 = -10.76 +2.46 * 16 = 28.60


*in rural area*\
$\hat{WAGE}$ where EDUC=12 = -4.88 +1.80 * 12 = 16.72\
$\hat{WAGE}$ where EDUC=16 = -4.88 +1.80 * 16 = 23.92


