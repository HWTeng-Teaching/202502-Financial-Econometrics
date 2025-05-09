# 2.1
## (a)
| (x) | (y) | (x - x̄) | (x - x̄)² | (y - ȳ) | (x - x̄)(y - ȳ) |
|----|----|----|----|----|----|
| 3  | 4  | 2  | 4  | 1.6  | 3.2  |
| 2  | 2  | 1  | 1  | -0.4  | -0.4  |
| 1  | 3  | 0  | 0  | 0.6  | 0  |
| -1  | 1  | -2  | 4  | -1.4  | 2.8  |
| 0  | 0  | -1  | 1  | -2.4  | 2.4  |
| **Σxi = 5**  | **Σyi = 10**  | **Σ(xi - x̄) = 0**  | **Σ(xi - x̄)² = 10**  | **Σ(yi - ȳ) = 0**  | **Σ(xi - x̄)(yi - ȳ) = 8**  |

## (b)

<div align="left">

$b_2 = \frac{\sum (x_i - \bar{x}) (y_i - \bar{y})}{\sum (x_i - \bar{x})^2} = 0.8$

b₂ 表示當 \( x \) 增加一單位，\( y \) 平均會增加 0.8 單位。

$b_1 = \bar{y} - b_2 \bar{x} = 1.2$

b₁ 表示當 \( x = 0 \) 時，\( y \) 預期為 1.2 單位。

</div>

## (c)

- Σ xᵢ² = 15
- Σ xᵢ yᵢ = 18
- x̄ = 1, ȳ = 2, N = 5

Σ(xᵢ - x̄)² = Σxᵢ² - N x̄²
   - 10 = 15 - (5 × 1²) = 10 

Σ(xᵢ - x̄)(yᵢ - ȳ) = Σxᵢyᵢ - N x̄ȳ
   - 8 = 18 - (5 × 1 × 2) = 8


## (d) 

已知回歸方程：
ŷᵢ = b₁ + b₂xᵢ  
其中 **b₁ = 1.2，b₂ = 0.8**，則：

| xᵢ  | yᵢ  | ŷᵢ  | eᵢ = yᵢ - ŷᵢ | eᵢ² | xᵢeᵢ |
|----|----|----|----|----|----|
| 3  | 4  | 3.6 | 0.4 | 0.16 | 1.2 |
| 2  | 2  | 2.8 | -0.8 | 0.64 |-1.6 |
| 1  | 3  | 2.0 | 1.0 | 1.00 | 1.0 |
| -1 | 1  | 0.4 | 0.6 | 0.36 | 0.6 |
| 0  | 0  | 1.2 | -1.2 | 1.44 | 0 |
| **Σ xᵢ = 5**  | **Σ yᵢ = 10**  |**Σ ŷᵢ = 10**| **Σ eᵢ = 0** | **Σ eᵢ² = 3.6** | **Σ xᵢ eᵢ = 0** |

 
s_y² = (Σ (yᵢ - ȳ)²) / (N-1) = 10 / 4 = 2.5


s_x² = (Σ (xᵢ - x̄)²) / (N-1) = 10 / 4 = 2.5


s_xy = (Σ (xᵢ - x̄)(yᵢ - ȳ)) / (N-1) = 8 / 4 = 2


r_xy = s_xy / (s_x * s_y) = 2 / (√2.5 * √2.5) = 0.8


CV_x = 100 * (s_x / x̄) = 100 * (√2.5 / 1) = 158.1%

 
中位數 = 1



## (e) 
![image](https://github.com/user-attachments/assets/9a692efa-b65e-429f-9a49-e4d66051137a)

ŷᵢ = 1.2 + 0.8xᵢ



## (f) 
均值點 (x̄, ȳ) = (1, 2)  
代入回歸方程：  
ŷ = 1.2 + 0.8(1) = 2 



## (g) 
ȳ = 2， b₁ + b₂x̄ = 1.2 + 0.8(1) = 2 



## (h) 
ȳ = Σ ŷᵢ / N = 10 / 5 = 2 



## (i)
σ̂² = Σ eᵢ² / (N-2) = 3.6 / (5-2) = 1.2



## (j) 
var(b₂ | x) = σ̂² / Σ (xᵢ - x̄)² = 1.2 / 10 = 0.12  
se(b₂) = √var(b₂ | x) = √0.12 = 0.346

# 2.14
![image](https://github.com/user-attachments/assets/db462c72-2482-48c0-bf7b-40773cc0effa)

# 2.16
![image](https://github.com/user-attachments/assets/986e7f2e-a967-438a-b816-ba28f99608f8)

## (b)
![image](https://github.com/user-attachments/assets/27b9c8c3-ede8-447f-a866-095700175b8e)
According to the result,ford is most aggressive and xom is most defensive.

## (c)
![image](https://github.com/user-attachments/assets/e35d6ff1-6dad-411d-913b-68c8d2992f9b)
不正確，alpha並不等於0
## (d)
![image](https://github.com/user-attachments/assets/a939c6d1-af58-40bc-b7af-55f5fd9d05c8)
結果差異不大


