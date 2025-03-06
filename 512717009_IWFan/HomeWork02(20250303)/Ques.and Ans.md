## Q.17
![image](https://github.com/user-attachments/assets/3b1c03f1-d8c9-4f84-930e-5edcdc69cab3)

## Ans:
**(a)**

![image](https://github.com/user-attachments/assets/7707aac1-b665-4dad-99fa-9bd43bd095e1)


--------------------------------------------------------------

**(b)**

![image](https://github.com/user-attachments/assets/3436bbc0-c2ce-4cab-9d39-3a6e6acb1b82)


*β₂ 的估計值表示房價（千美元）每增加一個單位的 SQFT（即100平方英尺）所增加的平均金額
*β₁ 為當 SQFT = 0 時的預測房價

--------------------------------------------------------------

**(c)**

### 對於居住面積為 2000 平方英尺的房屋，估計的邊際效應為 2(0.1845)20 = 7.3808。
如果其他所有因素保持不變，2000 平方英尺的房屋如果增加 100 平方英尺的居住面積，預期房價將上漲 7,380.80 美元。


*計算在 SQFT = 20 (即2000平方英尺，因為 20×100 = 2000) 下，每增加100平方英尺的邊際效應
*邊際效應: dprice/dSQFT = 2 * α₂ * SQFT

--------------------------------------------------------------
  
**(d)**

![image](https://github.com/user-attachments/assets/569b373a-8a9f-443a-a044-5c53def05116)


--------------------------------------------------------------

**(e)**

### 在2000平方英尺房屋下，房價相對於面積的彈性 = 0.8819511 

*彈性 = (d price/d sqft) * (sqft / price) evaluated at sqft_target

--------------------------------------------------------------

**(f)**

![image](https://github.com/user-attachments/assets/ef9ea346-8564-46dd-b2f8-8dac2e8006a0)

在這兩種模型中，殘差模式都不是隨機出現的。殘差的變化隨著 SQFT 的增加而增加，這表明同方差假設可能被違反。

--------------------------------------------------------------

**(g)**
### Linear Model SSE = 5,262,847 
### Quadratic Model SSE = 4,222,356 

殘差平方和線性關係為 5,262,847。二次關係的殘差平方和為 4,222,356。
較低的 SSE 對於二次模型比對於線性模型更接近擬合線。

## Q.25
![image](https://github.com/user-attachments/assets/7278f5e6-cddd-4cf7-82e5-bcd74b778f77)

## Ans:
**(a)** 

![image](https://github.com/user-attachments/assets/e2d4e725-da8f-4b7f-aee1-ec1da3eb9739)

foodaway_mean = 49.27085 

foodaway_median = 32.555 

foodaway_quant25 = 12.04 

foodaway_quant75 = 67.5025 

**結論**
FOODAWAY分佈右偏，高收入族群外食支出偏高。

--------------------------------------------------------------

**(b)**

| degree   | Mean_foodaway | Median_foodaway |
|----------|---------------|-----------------|
| none     | 39.0          | 26.0            |
| college  | 48.6          | 36.1            |
| advanced | 73.2          | 48.2            |

**結論**
有大學學歷家庭外食支出普遍較高。

--------------------------------------------------------------

**(c)** 

![image](https://github.com/user-attachments/assets/4416c50a-dff2-42ac-b0bf-442d4f17a5cf)


Summary:
  
| Min.    | 1st Qu. | Median | Mean   | 3rd Qu. | Max.   | NA's |
|---------|---------|--------|--------|---------|--------|------|
| -0.3011 | 3.0759  | 3.6865 | 3.6508 | 4.2797  | 7.0724 | 178  |

**結論**
ln(FOODAWAY)排除了0外食支出的家庭，樣本變少。ln後分佈較接近常態。

--------------------------------------------------------------

**(d)**

【Regression model estimation】
ln(FOODAWAY) =  3.1293  +  0.0069  * INCOME + e

* 當 INCOME 增加 1 單位（即100美元）時，ln(FOODAWAY) 平均變化 0.0069 單位。
  
**結論**
INCOME係數正，收入增加會提高外食支出。

--------------------------------------------------------------

**(e)** 

![image](https://github.com/user-attachments/assets/cabd0c66-543d-4049-9455-af19f936ea07)

**結論**
ln(FOODAWAY)與INCOME呈正相關。

--------------------------------------------------------------

**(f)** 

![image](https://github.com/user-attachments/assets/8da19990-47d1-45e3-b61a-b7dd4747e702)


OLS 殘差確實呈現隨機分佈，沒有明顯的模式。收入越高，觀察結果越少，因此「空白」就越多。




## Q.28
![image](https://github.com/user-attachments/assets/5c8f9c86-3739-4f76-8087-20360b029b26)

## Ans:
**(a)** 

* WAGE Summary:

| Min.    | 1st Qu. | Median | Mean   | 3rd Qu. | Max.   |
|---------|---------|--------|--------|---------|--------|
| 3.94 | 13.00  | 19.30 | 23.64 | 29.80  | 221.10 |

* EDUC Summary:

| Min.    | 1st Qu. | Median | Mean   | 3rd Qu. | Max.   |
|---------|---------|--------|--------|---------|--------|
|  0.0  | 12.0   | 14.0  | 14.2  | 16.0   | 21.0   |

![image](https://github.com/user-attachments/assets/6a5b2339-6197-4f93-86c6-d44560c0b745)

307 人接受了 12 年教育，這意味著他們在高中畢業時完成了學業。有少數觀察結果低於 12 歲，代表未完成高中學業的人。 16 歲時的峰值代表有 304 人完成了 4 年制大學學位，而 18 歲時的峰值代表有碩士學位，21 歲時的峰值代表有博士學位等進一步教育。 13 歲和 14 歲時，上過一兩年大學的人的發生率最高。

--------------------------------------------

**(b)**

【Linear Regression Results】

*Residuals:
   
|   Min   |   1Q    | Median  |   3Q   |   Max   |
|---------|---------|---------|--------|---------|
| -31.785 | -8.381  | -3.166  | 5.708  | 193.152 |
    

*Coefficients:
 
|    Term     | Estimate| Std. Error | t value   | Pr ( > abs(t) )|
|-------------|-----------|------------|---------|--------------|
| (Intercept) | -10.4000  | 1.9624     | -5.3    | 1.38e-07 *** |
| educ        | 2.3968    | 0.1354     | 17.7    | < 2e-16 ***  |
* Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

*Residual standard error: 13.55 on 1198 degrees of freedom

*Multiple R-squared:  0.2073,	

*Adjusted R-squared:  0.2067 

*F-statistic: 313.3 on 1 and 1198 DF,  

*p-value: < 2.2e-16

![image](https://github.com/user-attachments/assets/c8b30f10-0a36-4e36-973c-341a355be892)

係數 2.3968 表示額外一年教育帶來的預期小時工資率成長。 係數-10.4代表沒有受教育年限的工人的估計工資率。這不應被視為有意義，因為不可能出現負的時薪率。

--------------------------------------------

**(c)** 

![image](https://github.com/user-attachments/assets/a0b9d3bd-c4f8-40e0-b513-f8cf68a4d714)

隨著 EDUC 的增加，殘差的幅度也會增加，這表明 EDUC 值越大，誤差變異就越大
這違反了假設 **SR3（條件同質變異）**。如果假設 SR1-SR5 成立，則殘差中不應該存在任何明顯的模式。


--------------------------------------------

**(d)**


### 【 Regression for Males】

*Residuals:

|   Min    |   1Q    | Median  |   3Q   |   Max   |
|----------|---------|---------|--------|---------|
| -27.643  | -9.279  | -2.957  | 5.663  | 191.329 |
 
*Coefficients:

| Term                         | Estimate | Std. Error | t value | Pr ( > abs(t) ) |
|------------------------------|---------:|-----------:|--------:|-----------------|
| (Intercept)                  |  -8.2849 | 2.6738     | -3.099  |    0.00203 **   |
| educ (cps5_small$female == 0)|  2.3785  | 0.1881     | 12.648  |    < 2e-16 ***  |
* Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

*Residual standard error: 14.71 on 670 degrees of freedom

*Multiple R-squared:  0.1927,	

*Adjusted R-squared:  0.1915 

*F-statistic:   160 on 1 and 670 DF,  

*p-value: < 2.2e-16

### 【Regression for Females】

*Residuals:

|   Min   |   1Q    | Median  |   3Q   |   Max   |
|---------|---------|---------|--------|---------|
| -30.837 | -6.971  | -2.811  | 5.102  | 49.502  |


*Coefficients:

| Term                          | Estimate  | Std. Error | t value | Pr ( > abs(t) ) |
|-------------------------------|----------:|-----------:|--------:|-----------------|
| (Intercept)                   | -16.6028  | 2.7837     | -5.964  |    4.51e-09 *** |
| educ (cps5_small$female == 1) | 2.6595    | 0.1876     | 14.174  |    < 2e-16 ***  |
* Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

*Residual standard error: 11.5 on 526 degrees of freedom

*Multiple R-squared:  0.2764,	

*Adjusted R-squared:  0.275 

*F-statistic: 200.9 on 1 and 526 DF,  

*p-value: < 2.2e-16

### 【Regression for Blacks】

*Residuals:

|   Min   |   1Q    | Median  |   3Q   |   Max   |
|---------|---------|---------|--------|---------|
| -15.673 | -6.719  | -2.673  | 4.321  | 40.381  |


*Coefficients:

| Term                         | Estimate  | Std. Error | t value | Pr ( > abs(t) ) |
|------------------------------|----------:|-----------:|--------:|-----------------|
| (Intercept)                  |  -6.2541  |   5.5539   |  -1.126 |   0.263         |
| educ (cps5_small$black == 1) |   1.9233  |   0.3983   |   4.829 |   4.79e-06 ***  |         
* Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

*Residual standard error: 10.51 on 103 degrees of freedom

*Multiple R-squared:  0.1846,	

*Adjusted R-squared:  0.1767 

*F-statistic: 23.32 on 1 and 103 DF,  

*p-value: 4.788e-06

### 【Regression for Whites】

*Residuals:

|   Min   |   1Q    | Median  |   3Q   |   Max   |
|---------|---------|---------|--------|---------|
| -32.131 | -8.539  | -3.119  | 5.960  | 192.890 |


*Coefficients:

| Term                         | Estimate  | Std. Error | t value | Pr ( > abs(t) ) |
|------------------------------|----------:|-----------:|--------:|-----------------|
| (Intercept)                  | -10.475   | 2.081      | -5.034  |    5.6e-07 ***  |
| educ (cps5_small$black == 0) | 2.418     | 0.143      | 16.902  |    < 2e-16 ***  |
* Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

*Residual standard error: 13.79 on 1093 degrees of freedom

*Multiple R-squared:  0.2072,	

*Adjusted R-squared:  0.2065

*F-statistic: 285.7 on 1 and 1093 DF,  

*p-value: < 2.2e-16

從結果中我們可以看出，額外一年的教育對白人工人的預期工資率的提升作用比對黑人工人更大。
多接受一年教育可以提高女性工人的預期工資率，其提高幅度比男性工人更大。

--------------------------------------------

**(e)** 

【Quadratic Regression Results】

*Residuals:

|   Min   |   1Q    | Median  |   3Q   |   Max   |
|---------|---------|---------|--------|---------|
| -34.820 | -8.117  | -2.752  | 5.248  | 193.365 |

*Coefficients:

| Term         | Estimate  | Std. Error | t value | Pr ( > abs(t) ) |
|--------------|----------:|-----------:|--------:|--------------|
| (Intercept)  |  4.916477 |  1.091864  |  4.503  | 7.36e-06 *** |
| I(educ^2)    |  0.089134 |  0.004858  | 18.347  | < 2e-16 ***  |
* Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

*Residual standard error: 13.45 on 1198 degrees of freedom

*Multiple R-squared:  0.2194,	

*Adjusted R-squared:  0.2187 

*F-statistic: 336.6 on 1 and 1198 DF,  

*p-value: < 2.2e-16

![image](https://github.com/user-attachments/assets/1fb60857-3cc9-42ba-85f4-b774bcd2adf8)

### Marginal effect at 12 years of education = 2.139216 
### Marginal effect at 16 years of education = 2.852288 

*Compare these values with the linear model's estimated β2 (from part (b))
*The marginal effect in (b) for educ=12 and educ=16 are both 2.4

--------------------------------------------

**(f)** 

![image](https://github.com/user-attachments/assets/37954d58-653e-4533-93fc-fb3c35a0534b)


*Quadratic Regression Model fit the data better.
Because we can see this figure the linear model get negative wage,it doesn't make sense.
The quadratic model fit the points for EDUC<10 well.
