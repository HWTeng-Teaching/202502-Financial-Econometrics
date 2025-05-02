***學號：313704006     工工碩一 : 蘇柏誠***

<img width="552" alt="Image" src="https://github.com/user-attachments/assets/e3154fbd-84c6-4591-8b33-13f1637cb5d0" />

## (a)

<img width="719" alt="Image" src="https://github.com/user-attachments/assets/d7f5f468-8693-4d60-b83d-47d25ea3263b" />

## (b)
<img width="811" alt="Image" src="https://github.com/user-attachments/assets/b5cc1431-a1b6-4b4d-ae7a-148f513717a3" />
<img width="779" alt="Image" src="https://github.com/user-attachments/assets/628139dd-f367-4d61-b9f9-f7c87282b095" />

## (c)

```r
library(AER)
mroz.iv = ivreg(log(wage)~educ + exper + I(exper^2)| exper + I(exper^2) + mothercoll, data = married_data)
#confint(mroz.iv, level = 0.95)
vcoc_mroz = vcov(mroz.iv) 
mroz.iv$coefficients
df = mroz.iv$df.residual
tc = qt(0.975, df, lower.tail = TRUE)
educ_lb = mroz.iv$coefficients[2] - tc * (vcoc_mroz[2, 2]^0.5)
educ_ub = mroz.iv$coefficients[2] + tc * (vcoc_mroz[2, 2]^0.5)
cat("95% CI for educ coefficient: [", round(educ_lb, 5), ",", round(educ_ub, 5), "]\n")
```
<img width="828" alt="Image" src="https://github.com/user-attachments/assets/34779c60-0589-48d3-97b5-0c2e1ad8df20" />
