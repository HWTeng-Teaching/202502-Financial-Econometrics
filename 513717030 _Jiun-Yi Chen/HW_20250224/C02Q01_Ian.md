![圖片](https://github.com/user-attachments/assets/9fc18710-a938-4c37-af48-746a0eca4215)

# Solutions to Problem 2.1

## Step 1: Compute the Sample Means $\bar{x}$ and $\bar{y}$

We are given five observations:

x = {3, 2, 1, -1, 0}, y={4, 2, 3, 1, 0}

The means are calculated as:

$\bar{x}$ = $\frac{\sum x_i}{5}$ = $\frac{3 + 2 + 1 + (-1) + 0}{5}$ = $\frac{5}{5} = 1$

$\bar{y}$ = $\frac{\sum y_i}{5}$ = $\frac{4 + 2 + 3 + 1 + 0}{5}$ = $\frac{10}{5} = 2$

## (a) and (c) 
## Step 2: Complete the Table

| $x$ | $y$ | $x - \bar{x}$ | $(x - \bar{x})^2$ | $y - \bar{y}$ | $(x - \bar{x})(y - \bar{y})$ |
|--------|--------|----------------|-------------------|----------------|----------------------|
| 3      | 4      | 2              | 4                 | 2              | 4                    |
| 2      | 2      | 1              | 1                 | 0              | 0                    |
| 1      | 3      | 0              | 0                 | 1              | 0                    |
| -1     | 1      | -2             | 4                 | -1             | 2                    |
| 0      | 0      | -1             | 1                 | -2             | 2                    |

Summing the required columns:

$\sum (x - \bar{x})^2 = 4 + 1 + 0 + 4 + 1 = 10$

$\sum (y - \bar{y})^2 = 2^2 + 0^2 + 1^2 + (-1)^2 + (-2)^2 = 4 + 0 + 1 + 1 + 4 = 10$

$\sum (x - \bar{x})(y - \bar{y}) = 4 + 0 + 0 + 2 + 2 = 8$

## (b) 
## Step 3: Compute $b_1$ and $b_2$

The slope of the regression line is:

$b_1 = \frac{\sum (x - \bar{x})(y - \bar{y})}{\sum (x - \bar{x})^2} = \frac{8}{10} = 0.8$

The intercept is:

 $b_2 = \bar{y} - b_1 \bar{x} = 2 - (0.8 \times 1) = 2 - 0.8 = 1.2$

Thus, the regression equation is:

$\hat{y} = 0.8x + 1.2$

## (d)
## Step 4: Compute Fitted Values of y

Using the regression equation $\hat{y} = 0.8x + 1.2$:

| $x$ | $y$ | $\hat{y}$ |
|--------|--------|------------|
| 3      | 4      | $0.8(3) + 1.2 = 3.6$ |
| 2      | 2      | $0.8(2) + 1.2 = 2.8$ |
| 1      | 3      | $0.8(1) + 1.2 = 2.0$ |
| -1     | 1      | $0.8(-1) + 1.2 = 0.4$ |
| 0      | 0      | $0.8(0) + 1.2 = 1.2$ |

Summing the fitted values:

$\sum \hat{y} = 3.6 + 2.8 + 2.0 + 0.4 + 1.2 = 10$

|$x$             | $y$             | $\hat{y_{i}}$         |$\hat{e_{i}}$           | $\hat{e_{i}}^2$           |$x_{i}\hat{e_{i}}$               |
|----------------|-----------------|-----------------------|------------------------|---------------------------|---------------------------------|
|3               |4                | 3.6                   | 0.4                    |0.16                       |	1.2                            |
|2               |2                | 2.8                   |-0.8                    |0.64                       | -1.6                            |
|1               |3                | 2.0                   | 1.0                    |1                          |	1                              |
|-1              |1                | 0.4                   | 0.6                    |0.36                       | -0.6                            |
|0               |0                | 1.2                   |-1.2                    |1.44                       |	0                              |
|$\sum{x_{i}}$ =5|$\sum{y_{i}}$ =10|$\sum{\hat{y_{i}}}$ =10|$\sum{\hat{e_{i}}}$=0   |$\sum{\hat{e_{i}}^2}$ =3.6 |$\sum{x_{i}\hat{e_{i}}}$=0       |  

## Step 5: Compute Variance, Covariance, and Correlation

### Sample Variance of x 

$s_x^2 = \frac{\sum (x - \bar{x})^2}{N-1} = \frac{10}{4} = 2.5$

### Sample Variance of y

$s_y^2 = \frac{\sum (y - \bar{y})^2}{N-1} = \frac{10}{4} = 2.5$

### Sample Covariance between x and y 

$s_{xy} = \frac{\sum (x - \bar{x})(y - \bar{y})}{N-1} = \frac{8}{4} = 2$

### Sample Correlation Coefficient

$r_{xy} = \frac{s_{xy}}{s_x s_y} = \frac{2}{\sqrt{2.5} \times \sqrt{2.5}} = \frac{2}{2.5} = 0.8$

### Coefficient of Variation of x

$CV_x = \frac{s_x}{\bar{x}} \times 100 = \frac{\sqrt{2.5}}{1} \times 100 = \sqrt{2.5} \times 100 \approx 158.11$

## Step 6: Compute the Median of  x  (50th Percentile)

Sorting  x  values:  -1, 0, 1, 2, 3 . Since we have an odd number of values N=5 , the median is the middle value:

$\text{Median of }x = 1$
