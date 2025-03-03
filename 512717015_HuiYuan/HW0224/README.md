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

## (h)

## (i)

## (j)

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

| 教育年數 | Urban 預測工資 | Rural 預測工資 |
|----------|--------------|--------------|
| 12 年    | $18.76      | $16.72      |
| 16 年    | $28.60      | $23.92      |

# 2.16
![image](https://github.com/user-attachments/assets/0aa05018-cc67-460b-a818-9747f784a27d)

## (a)
$r_j-r_f$是超額報酬，受到風險溢酬的影響，是簡回歸裡的依變數  
$r_m-r_f$是市場風險溢酬，是簡回歸裡的自變數  
$\beta_j$是投組對市場風險溢酬的敏感度，是簡回歸裡的斜率 

## (b)
Most Aggressive Firm: Ford， $\beta_j$值為1.66  
Most Defensive Firm: Exxon-Mobil， $\beta_j$值為0.46  

## (c)


## (d)

