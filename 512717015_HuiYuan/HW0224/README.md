# 2.1
![image](https://github.com/user-attachments/assets/4755d591-cf99-4f36-8c37-d04ea1ef8d9b)

## (a)
| x   | y   | $x - \bar{x}$ | $(x - \bar{x})^2$ | $y - \bar{y}$ | $(x - \bar{x})(y - \bar{y})$ |
|-----|-----|-------------|-----------------|-------------|----------------------------|
| 3   | 4   |      2      |       4         |      2      |           4                |
| 2   | 2   |      1      |       1         |      0      |           0                |
| 1   | 3   |      0      |       0         |      1      |           0                |
| -1  | 1   |     -2      |       4         |      -1     |           2                |
| 0   | 0   |     -1      |       1         |      -2     |           2                |
| $\sum x_i=5$ | $\sum y_i=10$ | $\sum(x_i - \bar{x})=0$ | $\sum(x_i - \bar{x})^2=10$ | $\sum(y_i - \bar{y})=0$ | $\sum(x_i - \bar{x})(y_i - \bar{y})=8$ |

**Note: $\bar{x}$ = 1 ; $\bar{y}$ = 2**

## (b)
$b_2=\frac{\sum (x_i - \bar{x})(y_i - \bar{y})}{\sum (x_i - \bar{x})^2}=\frac{8}{10}=0.8$\
$$b_1=\bar{y}-b_2\bar{x}=2-0.8*1=1.2$$ 

## (c)
$\sum (x_i - \bar{x})^2=\sum{x_i}^2-N\bar{x}^2=10$   
$\sum (x_i - \bar{x})(y_i - \bar{y})=\sum(x_i y_i)-N\bar{x}\bar{y}=8$   

## (d)
| $x_i$   | $y_i$   | $\hat{y}_i$ | $\hat{e}_i$ | $\hat{e}_i^2$ | $x_i \hat{e}_i$ |
|-----|-----|-------------|-----------------|-------------|----------------------------|
| 3   | 4   |     3.6      |      0.4        |      0.16    |           1.2               |
| 2   | 2   |     2.8      |       -0.8      |       0.64   |           -1.6              |
| 1   | 3   |      2       |        1        |      1       |           1                 |
| -1  | 1   |      0.4     |        0.6      |      0.36    |           -0.6              |
| 0   | 0   |      1.2     |       -1.2      |      1.44    |           0                 |
| $\sum{x}_i=5$ | $\sum{y}_i=10$ | $\sum\hat{y}_i=10$ | $\sum\hat{e}_i=0$ | $\sum\hat{e}_i^2=3.6$ | $\sum{x}_i \hat{e}_i=0$ |

$s_y^2 = \frac{\sum (y_i - \bar{y})^2}{N - 1}=2.5$  
$s_x^2 = \frac{\sum (x_i - \bar{x})^2}{N - 1}=2.5$  
$s_{xy} = \frac{\sum (y_i - \bar{y})(x_i - \bar{x})}{N - 1}=2$   
$r_{xy} = \frac{s_{xy}}{s_x s_y}=0.8$  
$CV_x = 100 \times \frac{s_x}{\bar{x}}=158.144$  
$x$的中位數為1  

## (e).(f)
![image](https://github.com/user-attachments/assets/edc6dbb9-badf-420d-9092-354f28a15746)

## (g)
$\bar{x}$ = 1

$\bar{y}$ = 2

$b_{1}$ = 1.2

$b_{2}$ = 0.8

then $\bar{y} = b_{1} + b_{2} \bar{x}$ = 1.2 +0.8*1 = 2

## (h)
We known $\sum{\hat{y}}$ = 10 from d. ,then  $\bar{\hat{y}}$ = 2 .

And we known $\sum{y}$ = 10 from a. ,then  $\bar{y}$ = 2 .

So, $\bar{y}$ = $\bar{\hat{y}}$

## (i)
SSE = $\sum{\hat{e_{i}}^2}$ =3.6

$\hat{\sigma}^2 = SSE / N-2 = 3.6 / 3 = 1.2 $

## (j)
$\hat{Var}(b_2 | x)$ = $\hat{\sigma}^2$ / $\sum{(x_{i}-\bar{x})^2}$ = 1.2 / 10 = 0.12

$se(b_2)$ = $\sqrt{\hat{Var}(b_2 | x)}$ = 0.3464


# 2.14
![image](https://github.com/user-attachments/assets/79c87570-23a2-4e65-9b6b-492ea2d15262)

## (a)
$19.74 = -4.88 + 1.80 \bar{EDUC}$  
解出 $\bar{EDUC}$：
$1.80 \bar{EDUC} = 19.74 + 4.88 = 24.62$  
$\bar{EDUC} = \frac{24.62}{1.80} \approx 13.68$  
$Elasticity = 1.80 \times \frac{13.68}{19.74}\approx 1.248$

## (b)
$\bar{WAGE} = -10.76 + 33.65 = 22.89$  
$Elasticity = \beta_2 \times \frac{\bar{EDUC}}{\bar{WAGE}}$  
$Elasticity = 2.46 \times \frac{13.68}{22.89}$  
$Elasticity = 2.46 \times 0.5979 = 1.47$  
$se(Elasticity) = se(\beta_2) \times \frac{\bar{EDUC}}{\bar{WAGE}}$  
$se(Elasticity) = 0.16 \times \frac{13.68}{22.89}$  
$se(Elasticity) = 0.16 \times 0.5979 = 0.0957$  

## (c)
URBAN: $WAGE = -10.76 + 2.46 EDUC$
當 EDUC = 12 ：  
$WAGE = -10.76 + 2.46(12) = -10.76 + 29.52 = 18.76$   
當 EDUC = 16 ：  
$WAGE = -10.76 + 2.46(16) = -10.76 + 39.36 = 28.60$  
RURAL: $WAGE = -4.88 + 1.80 EDUC$  
當 EDUC = 12 ：
$WAGE = -4.88 + 1.80(12) = -4.88 + 21.60 = 16.72$  
當 EDUC = 16 ：
$WAGE = -4.88 + 1.80(16) = -4.88 + 28.80 = 23.92$  

| Year | Urban  | Rural  |
|----------|--------------|--------------|
| 12 年    | $18.76      | $16.72      |
| 16 年    | $28.60      | $23.92      |

# 2.16
![image](https://github.com/user-attachments/assets/0aa05018-cc67-460b-a818-9747f784a27d)

## (a)
The CAPM econometric version can be written as

$$
r_{j} - r_f = \beta_j (r_m - r_f)
$$
->
$$
r_{j} = \alpha_j + \beta_j (r_m - r_f) + e_j
$$

where we define

$$
y = r_j - r_f \quad \text{and} \quad x = r_m - r_f
$$

The original CAPM econometric model and by substituting the definitions above, we can rewrite the model as:
:

$$
y  = \alpha_j + \beta_j x + e_j
$$

then

$$
y = \beta_1 + \beta_2 x + e,
$$

with the identifications

$$
\beta_1 = \alpha_j \quad \text{and} \quad \beta_2 = \beta_j.
$$

Since this formulation involves only one independent variable x and one dependent variable y, it qualifies as a simple regression model.

## (b)
### CAPM Regressions for 6 Stocks
| **Parameter**    | **GE**     | **IBM**    | **FORD**   | **MSFT**   | **DIS**    | **XOM**    |
|------------------|------------|------------|------------|------------|------------|------------|
| **alpha_hat**    | -0.000959  | 0.006053   | 0.003779   | 0.003250   | 0.001047   | 0.005284   |
| (alpha_se)       | (0.0044)   | (0.0048)   | (0.0102)   | (0.0060)   | (0.0047)   | (0.0035)   |
| **beta_hat**     | 1.147952   | 0.976890   | 1.662031   | 1.201840   | 1.011521   | 0.456521   |
| (beta_se)        | (0.0895)   | (0.0978)   | (0.2069)   | (0.1222)   | (0.0946)   | (0.0716)   |
| **N**            | 180        | 180        | 180        | 180        | 180        | 180        |

### Conclusion
From the table above, the beta values for each stock are as follows:
- **GE**: beta ~ 1.148
- **IBM**: beta ~ 0.977
- **FORD**: beta ~ 1.662
- **MSFT**: beta ~ 1.202
- **DIS**: beta ~ 1.012
- **XOM**: beta ~ 0.457

A beta greater than 1 indicates that the stock is more sensitive to market fluctuations and is considered more "aggressive"; whereas a beta less than 1 suggests it is more "defensive".

It can be observed that:

- **FORD, GE, and MSFT** have beta values greater than 1, indicating a more aggressive stance, with **FORD (beta ~ 1.662)** being the most aggressive.
- **IBM, DIS, and XOM** have beta values less than 1, categorizing them as more defensive, with **XOM (beta ~ 0.457)** being the most defensive.

## (c)
![image](https://github.com/user-attachments/assets/2b94571b-42aa-43e3-b3c4-f2a363fc54c2)

## (d)
### The estimates for βj given 0 αj = are as follows:
| **Parameter**        | **beta_original** | **beta_noint** |
|----------------------|-------------------|----------------|
| GE.mkt_excess        | 1.1479521         | 1.1467633      |
| IBM.mkt_excess       | 0.9768898         | 0.9843954      |
| FORD.mkt_excess      | 1.6620307         | 1.6667168      |
| MSFT.mkt_excess      | 1.2018398         | 1.2058695      |
| DIS.mkt_excess       | 1.0115207         | 1.0128190      |
| XOM.mkt_excess       | 0.4565208         | 0.4630727      |

*The restriction αj = 0 has led to small changes in the βj ; but it has not changed the aggressive 
or defensive nature of the stock.  

