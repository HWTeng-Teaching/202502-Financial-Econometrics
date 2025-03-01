## Q.01
![image](https://github.com/user-attachments/assets/93ff4c71-2084-4039-8ee2-9e0d90cce888)

## Ans:
### (a) Complete the entries in the table

| x | y | x - x̄ | (x - x̄)² | y - ȳ | (x - x̄)(y - ȳ) |
|----|----|----|----|----|----|
| 3 | 4 | 2 | 4 | 2 | 4 |
| 2 | 2 | 1 | 1 | 0 | 0 |
| 1 | 3 | 0 | 0 | 1 | 0 |
| -1 | 1 | -2 | 4 | -1 | 2 |
| 0 | 0 | -1 | 1 | -2 | 2 |
| 5 | 10 | 0 | 10 | 0 | 8 |

Mean x̄ = (3+2+1-1+0)/5 = 1

Mean ȳ = (4+2+3+1+0)/5 = 2

Σxi = 5

Σyi = 10

Σ(x - x̄)^2 = 10

Σ(y - ȳ)^2 = 10

Σ(x - x̄)(y - ȳ) = 8

### (b) 
b2 = Σ(x - x̄)(y - ȳ) / Σ(x - x̄)^2 = 8 / 10 = 0.8

b1 = ȳ - b2 * x̄ = 2 - 0.8 * 1 = 1.2

### (c) 
Σxi² = 3² + 2² + 1² + (-1)² + 0² = 15

Σ(xi - x̄)² = Σxi² - N * x̄² = 15 - 5 * 1² = 10

The same as in part (a), the verification holds.

Σxiyi = 3*4 + 2*2 + 1*3 + (-1)*1 + 0*0 = 18

Σ(x - x̄)(y - ȳ) = Σxiyi - N * x̄ * ȳ = 18 - 5*1*2 = 8

The same as in part (a), the verification holds.

### (d) 
x  | y  | ŷ  | e^  | e^2 | x*e
|----|----|----|----|----|----|
| 3 | 4 | 3.6 | 0.4 | 0.16 | 1.2 |
| 2 | 2 | 2.8 | -0.8 | 0.64 | -1.6 |
| 1 | 3 | 2.0 | 1.0 | 1.00 | 1.0 |
| -1 | 1 | 0.4 | 0.6 | 0.36 | -0.6 |
| 0 | 0 | 1.2 | -1.2 | 1.44 | 0.0 |
| 5 | 10 | 10 | 0 | 3.6 | 0 |

sy² = Σ(y - ȳ)² / (N-1) = 10 / 4 = 2.5

sx² = Σ(x - x̄)² / (N-1) = 10 / 4 = 2.5

sxy = Σ(x - x̄)(y - ȳ) / (N-1) = 8 / 4 = 2

rxy = sxy / (sx * sy) = 2 / (√2.5 * √2.5) = 2 / 2.5 = 0.8

CVx = 100 * (√2.5 / 1) = 158.11388

median( x )  = 1 


### (e)
![image](https://github.com/user-attachments/assets/0685f7df-e29f-4f8f-8d26-f3c870e29a2a)

### (f)
x̄ = 1    ȳ = 2

ŷ = 1.2 + 0.8x

ŷ = 1.2 + 0.8(1) = 2

Conclusion: The regression line passes through the point of means (1, 2)
![image](https://github.com/user-attachments/assets/f418d4e9-8bce-4466-8ba5-092b9c45f3bc)


### (g)
ȳ = b1 + b2 * x̄

b1 = 1.2

b2 = 0.8

x̄ = 1

ȳ = b1 + b2 * x̄ = 1.2 + 0.8 * 1 = 2

ȳ = 2

Conclusion: The verification holds.

### (h)
Verification  ŷ = ȳ

Prediction  ŷi:
x = 3, ŷ = 3.6
x = 2, ŷ = 2.8
x = 1, ŷ = 2.0
x = -1, ŷ = 0.4
x = 0, ŷ = 1.2

ŷ = (3.6 + 2.8 + 2.0 + 0.4 + 1.2) / 5 = 2

ȳ = 2

Conclusion: The verification holds.

### (i)
 (RSS)
| x  | y  | ŷ  | e = y - ŷ | e² |
|----|----|----|----|----|
3  | 4  | 3.6 | 0.4 | 0.16
2  | 2  | 2.8 | -0.8 | 0.64
1  | 3  | 2.0 | 1.0 | 1.00
-1 | 1  | 0.4 | 0.6 | 0.36
0  | 0  | 1.2 | -1.2 | 1.44

e² = 3.6

σ^2 = SSE/N−2=3.6/3=1.2

### (j)
$\hat{Var}(b_2 | x)$ = $\hat{\sigma}^2$ / $\sum{(x_{i}-\bar{x})}$ = 1.2 / 10 = 0.12

$se(b_2)$ = $\sqrt{\hat{Var}(b_2 | x)}$ = 0.3464





## Q.14
![image](https://github.com/user-attachments/assets/b5c78667-3cee-439f-b464-4d6370d4b129)

## Ans:
### (a)
WAGĒ = 19.74

WAGÊ = -4.88 + 1.80 * EDUC

EDUC̄ = (19.74 + 4.88) / 1.80 = 13.6778

Elasticity = β2 * (EDUC̄ / WAGĒ)

Elasticity = 1.80 * (13.6778 / 19.74) = 1.2472

Elasticity ≈ 1.2472

### (b)

WAGÊ = -10.76 + 2.46 * EDUC

EDUC̄ = 13.68

WAGĒ = -10.76 + 2.46 * 13.68 = 22.8928

Elasticity = β2 * (EDUC̄ / WAGĒ)

Elasticity = 2.46 * (13.68 / 22.8928) = 1.470

SE(ε̂) = (EDUC̄ / WAGĒ) * SE(β2)

SE(ε̂) = (13.68 / 22.8928) * 0.16 = 0.0956

SE(ε̂) = 0.0956

### (c)
Urban Area

regression model:
WAGÊ = -10.76 + 2.46 * EDUC

- EDUC = 12:
WAGÊ = -10.76 + 2.46 * 12 = 18.76

- EDUC = 16:
WAGÊ = -10.76 + 2.46 * 16 = 28.60

Rural Area

regression model:
WAGÊ = -4.88 + 1.80 * EDUC

- EDUC = 12:
WAGÊ = -4.88 + 1.80 * 12 = 16.72

- EDUC = 16:
WAGÊ = -4.88 + 1.80 * 16 = 23.92

| 教育年數 | Urban預測工資 | Rural預測工資 |
|----|----|----|
| 12年 | 18.76 | 16.72 |
| 16年 | 28.60 | 23.92 |




## Q.16
![image](https://github.com/user-attachments/assets/37c9bd07-de49-4e89-a514-446189c83afa)
## Ans:
## (a) 
- y = α + βx + e
- y = stock excess return
- x = market excess return

- $y = r_j + r_f , x = r_m - r_f , \beta_1 = \alpha ,\beta_2 = \beta_j$
- This is exactly a simple regression form.

## (b) 
| Firm       | α̂j      | SE(α̂j) | β̂j   | SE(β̂j) | N  |
|------------|--------|------|-----|------|----|
| GE         | -0.000959 | 0.00442 | 1.148 | 0.0895 | 180 |
| IBM        | 0.00605  | 0.00483 | 0.977 | 0.0978 | 180 |
| Ford       | 0.00378  | 0.0100  | 1.662 | 0.207  | 180 |
| Microsoft  | 0.00325  | 0.00604 | 1.202 | 0.122  | 180 |
| Disney     | 0.00105  | 0.00468 | 1.012 | 0.0946 | 180 |
| ExxonMobil | 0.00528  | 0.00354 | 0.457 | 0.0716 | 180 |

### Interpretation

- Most Aggressive: **Ford** (β = 1.662)
- Relatively Aggressive: **GE**, **Microsoft**
- Relatively Defensive: **Disney**, **IBM**
- Most Defensive: **ExxonMobil** (β = 0.457)

### Notes
- β > 1: Aggressive (more volatile than market)
- β < 1: Defensive (less volatile than market)
- α should theoretically be 0 under CAPM. Small deviations indicate market inefficiency or model misspecification.

## (c) 
α̂ = 0.00325, SE(α̂) = 0.00604
β̂ = 1.202, SE(β̂) = 0.122

Interpretation of α̂:
- CAPM theory says α = 0.
- Estimated α̂ = 0.00325 is very close to 0.
- Standard error (0.00604) > estimate itself, so α is statistically insignificant.
- This supports CAPM’s assumption that α = 0 in efficient markets.

Scatter Plot & Fitted Line:
- Scatter plot (Figure xr2.15) shows Microsoft excess returns vs. market excess returns.
- Fitted line passes close to origin, consistent with α close to zero.
- Positive slope reflects β > 1 (Microsoft is slightly aggressive).

Conclusion:
- α̂ is close to zero and insignificant, consistent with CAPM.
- Fitted line visually supports the regression estimates.
  
![image](https://github.com/user-attachments/assets/1503c424-1d1b-403d-96fb-3a9edb867390)

## (d) 
#### Re-estimated Betas (α = 0)

| Firm       | β̂ (α=0) | SE |
|------------|----|----|
| GE         | 1.147 | 0.0891 |
| IBM        | 0.984 | 0.0978 |
| Ford       | 1.667 | 0.206 |
| Microsoft  | 1.206 | 0.122 |
| Disney     | 1.013 | 0.0942 |
| ExxonMobil | 0.463 | 0.0717 |

#### Comparison with Original (Part b)

| Firm       | Original β̂ (with α) | β̂ (α=0) | Change |
|------------|----|----|----|
| GE         | 1.148 | 1.147 | -0.001 |
| IBM        | 0.977 | 0.984 | +0.007 |
| Ford       | 1.662 | 1.667 | +0.005 |
| Microsoft  | 1.202 | 1.206 | +0.004 |
| Disney     | 1.012 | 1.013 | +0.001 |
| ExxonMobil | 0.457 | 0.463 | +0.006 |

#### Interpretation
- Very minor changes in β̂.
- Classification of aggressive vs defensive stocks **remains unchanged**.
- This is expected since α̂ was very close to zero in part (b) and (c).

#### Conclusion
✅ Forcing α = 0 does not materially impact the beta estimates.
✅ Stock risk classifications remain unchanged.
