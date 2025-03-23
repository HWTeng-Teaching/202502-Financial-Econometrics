# 4.4
![image](https://github.com/user-attachments/assets/9414dbfc-a1b6-4eaf-a226-3d70a51ee420)
## (a)&(b)   
![image](https://github.com/user-attachments/assets/657999ce-70ad-439f-9055-80416466e4e6)
Model 1: 一條斜率為 0.990 的直線，代表每增加一年經驗，平均表現評分增加約 0.99 分。   
Model 2: 使用了對數轉換，In(EXPERT)，因為In(0)未被定義，所以無法對沒有經驗的人進行預測。(只有46位藝術家被納入模型2的估計)   
R Code:   
![image](https://github.com/user-attachments/assets/00fa15ca-dd0b-492b-a428-669bf8efc492)
## (c)
Model 1 為線性模型，邊際效益為固定(斜率) = 0.99   
## (d)
公式: dRATING / dEXPERT   
10年: 15.312 / 10 = 1.531   
20年: 15.312 / 20 = 0.7656   
表示：在模型2中，經驗初期對表現有較大幫助，隨著年資增加，效益遞減。   
## (e)
Model 2: R2 = 0.6414 > Model 1: R2 = 0.3793，所以數據擬合較佳。   

## (f)
模型2更符合邊際報酬遞減（Diminishing Returns）的經濟理論。在初期階段，增加經驗能顯著提升表現；但隨著經驗累積，新增一年的效果會越來越小。   

------
# 4.28
![image](https://github.com/user-attachments/assets/e0e84eb5-d215-4be5-8137-baf0ef8b45ff)
## (a)
![image](https://github.com/user-attachments/assets/f49916d4-7352-45c5-bcfe-91a3524579c6)   
![image](https://github.com/user-attachments/assets/9d81f29a-fc68-48d1-9344-73de64518ce8)   
![image](https://github.com/user-attachments/assets/1095b92d-2457-4f57-bf13-4f07a7744ea7)
The quadratic model has the highest p-value, indicating that its residuals are closest to a normal distribution and therefore best satisfy the normality assumption.The mode3 (Quadratic Model) is preferred.   
## (b)
For the quadratic model, the estimated coefficient for the squared time term is 0.0004986. Since the marginal effect is calculated as the derivative of the yield with respect to time, it equals 2×0.0004986×time. This means the slope—and thus the marginal effect—changes at each time point.   
When evaluated at time = 10, 20, 30, and 40, the corresponding marginal effects on yield are approximately 0.00997, 0.01994, 0.02992, and 0.03989, respectively. These values show that the marginal effect of time on yield increases as time progresses. This suggests that, over time, the accumulated impact of technological advancements leads to greater increases in output, resulting in a rising marginal effect.   
## (c)
![image](https://github.com/user-attachments/assets/dc087de1-8737-42d8-aef4-9c0dcb62bd33)   
Outlier Observation Table:   
The year 1963 (TIME = 14, YIELD = 0.3024) is identified as a low-yield outlier that has a significant impact on the regression results. Similarly, the year 1992 (TIME = 43, YIELD = 2.3161) stands out as an unusually high-yield point, which also greatly influences the regression coefficients.   
These outliers may have been caused by extreme weather conditions, policy changes, or technological breakthroughs. Outlier detection indicates that both 1963 (low yield) and 1992 (high yield) exert substantial influence on the regression analysis.   
## (d)
We used the quadratic model to predict the yield for the year 1997, along with a 95% confidence interval.   
Predicted yield for 1997: 2.2318 tons per hectare   
95% Confidence Interval: [1.3724, 2.3898]   
This means we are 95% confident that the actual yield in 1997 falls within this reasonable confidence range.   

------
# 4.29
![image](https://github.com/user-attachments/assets/8e310906-de2c-4fe8-a0e8-576293f1f2cf)
## (a)
![image](https://github.com/user-attachments/assets/8f1f9f01-bfba-4959-a831-028c9fb494e2)   
![image](https://github.com/user-attachments/assets/0f07e47a-3cf5-417c-ac2d-19ea333ad3de)   
![image](https://github.com/user-attachments/assets/141825aa-7518-4be6-a309-4921b7f12a3e)   
## (b)
![image](https://github.com/user-attachments/assets/1c65ac09-fd3a-4c91-9ec0-ecb456337e71)
![image](https://github.com/user-attachments/assets/a3e424ed-c2c9-4f72-b39a-ed04bf7019fa)
## (c)
![image](https://github.com/user-attachments/assets/ddc016c8-e559-4ad5-9f14-c51cb360eef2)
## (d)
![image](https://github.com/user-attachments/assets/19b55e63-3d87-4bea-9c28-d1d2a93d3130)   
## (e)
![image](https://github.com/user-attachments/assets/59c9a6fa-36b8-4c0e-8ebc-c012a8900db2)   
## (f)
The 95% confidence interval for elasticity is estimated to be [0.1293, 0.2433], which is similar to the interval estimated at INCOME = 65. This value approximately represents the median of the income distribution.   
However, the elasticity interval from the log-log model does not overlap with the interval estimates from the linear model at INCOME = 19 (approximately the 5th percentile) or INCOME = 160 (approximately the 95th percentile). This indicates that if we test whether the elasticity from the log-log model falls within the confidence interval of the linear model at either INCOME = 19 or INCOME = 160, we would reject the null hypothesis at the 5% significance level.
## (g)
![image](https://github.com/user-attachments/assets/63ff8119-1f32-4577-a3f4-db52fa0d4609)   
## (h)
![image](https://github.com/user-attachments/assets/5d8d32ef-9ef8-4461-b88f-f31bbaa78f6d)   
## (i)
![image](https://github.com/user-attachments/assets/ac42fd0a-6692-448e-83db-cdc8a8885051)
## (j)
![image](https://github.com/user-attachments/assets/df9efba5-91db-4ab5-a95b-e2b82e7bac9f)
## (k)
As income elasticity increases, the linear model becomes counterintuitive. While the linear-log model aligns with economic reasoning, its residual pattern does not exhibit an ideal random distribution.   
On the other hand, the log-log model assumes constant income elasticity across all income levels—a reasonable assumption—and its residuals appear to be the most randomly dispersed. Additionally, based on skewness and kurtosis, the log-log model shows the least deviation from normality in residuals.   
For these reasons, the log-log model appears to be a strong candidate for representing the relationship accurately.   

