# 計量經濟作業

### a.
| x   | y   | x - x̄ | (x - x̄)² | y - ȳ | (x - x̄)(y - ȳ) |
|-----|-----|-------|----------|-------|----------------|
| 3   | 4   |  2    |   4      |   2   |       4        |
| 2   | 2   |  1    |   1      |   0   |       0        |
| 1   | 3   |  0    |   0      |   1   |       0        |
| -1  | 1   | -2    |   4      |  -1   |       2        |
| 0   | 0   | -1    |   1      |  -2   |       2        |
| **Σx<sub>i</sub> = 5**  | **Σy<sub>i</sub> = 10**  | **Σ(x<sub>i</sub>-x&#772;) = 0** | **Σ(x<sub>i</sub>-x&#772;)&#178; = 10** | **Σ(y<sub>i</sub>-y&#772;) = 0** | **Σ(x<sub>i</sub>-x&#772;)(y<sub>i</sub>-y&#772;) = 8** |

$$
\bar{X} = 1, \quad \bar{Y} = 2
$$

### b.
$$
b_2 = \frac{\sum (X_i - \bar{X})(Y_i - \bar{Y})}{\sum (X_i - \bar{X})^2} = \frac{8}{10} = 0.8
$$
$$
b_1 = \bar{Y} - b_2 \bar{X} = 2 - (1 \times 0.8) = 1.2
$$

### c.
$$
\sum (X_i - \bar{X})^2 = \sum (X_i^2 - 2X_i\bar{X} + \bar{X}^2)
$$
$$
= \sum X_i^2 - 2\bar{X} \sum X_i + N \bar{X}^2
$$
$$
= \sum X_i^2 - \frac{1}{N} (\sum X_i)^2
$$
$$
= \sum X_i^2 - N\bar{X}^2
$$

$$
\sum (X_i - \bar{X})(Y_i - \bar{Y}) = \sum X_i Y_i - \bar{X} \sum Y_i - \bar{Y} \sum X_i + N\bar{X}\bar{Y}
$$
$$
= \sum X_i Y_i - \frac{1}{N} \sum X_i \sum Y_i - \frac{1}{N} \sum X_i \sum Y_i + N\bar{X}\bar{Y}
$$
$$
= \sum X_i Y_i - N\bar{X}\bar{Y}
$$

### d.

$$
\sum (Y_i - \bar{Y})^2 = \sum Y_i^2 - N\bar{Y}^2
$$
$$
= 30 - 5 \times 2^2
$$
$$
= 10
$$

$$
S_Y^2 = \frac{10}{4} = 2.5
$$

$$
S_X^2 = \frac{10}{4} = 2.5
$$

$$
S_{XY} = \frac{8}{4} = 2
$$

$$
r_{XY} = \frac{S_{XY}}{S_X S_Y} = \frac{2}{\sqrt{2.5 \times 2.5}} = 0.8
$$

$$
CV_X = 100 \times \left( \frac{\sqrt{2.5}}{1} \right) = 158.114
$$

$$
\text{Median of } X = 1, \quad \text{50th percentile} = 1
$$

### e.f.
![image](https://github.com/user-attachments/assets/c612830d-319b-49be-b680-c7f9216d2df6)

$$
\text{迴歸線必通過} \quad \bar{X}, \quad \bar{Y} 
$$

### g.
$$
\hat{Y}_i = b_1 + b_2 X_i
$$
$$
\sum Y_i = n b_1 + b_2 \sum X_i
$$
$$
\frac{1}{n} \sum Y_i = b_1 + \frac{1}{n} b_2 \sum X_i
$$
$$
\bar{Y} = b_1 + b_2 \bar{X}
$$

### h.
$$
\bar{Y} = \frac{1}{n} \sum \hat{Y}_i = \frac{1}{n} \sum (b_1 + b_2 X_i)
$$
$$
= \frac{1}{n} \times n b_1 + \frac{1}{n} b_2 \sum X_i
$$
$$
= b_1 + b_2 \bar{X} = \bar{Y}
$$


### i.
$$
\hat{\sigma}^2 = \frac{\sum e_i^2}{n-2} = \frac{3.6}{3} \approx 1.2
$$

## j.
$$
\hat{\text{Var}}(b_2 | x) = \frac{\hat{\sigma}^2}{S_{xx}} = \frac{1.2}{10} = 0.12
$$

$$
\text{se}(b_2 | x) = \sqrt{\hat{\text{Var}}(b_2 | x)} = 0.3464
$$
