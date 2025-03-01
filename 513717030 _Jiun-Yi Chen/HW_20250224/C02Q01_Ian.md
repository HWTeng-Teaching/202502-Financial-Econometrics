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

## (e)
![圖片](https://github.com/user-attachments/assets/7b6eb60b-364b-4d73-a217-5e518dbd2579)

R 
```R Language
# Define the data points
x <- c(3, 2, 1, -1, 0)
y <- c(4, 2, 3, 1, 0)

# Compute the fitted values based on the regression equation
y_hat <- 0.8 * x + 1.2

# Compute the mean point (x̄, ȳ)
x_mean <- mean(x)
y_mean <- mean(y)

# Set up plot limits (adjusted to ensure the regression line fits)
x_min <- min(x) - 1
x_max <- max(x) + 1
y_min <- min(y) - 1
y_max <- max(y) + 1

# Open plotting space and adjust margins for legend outside the plot
par(mar = c(4, 4, 5, 2))  # Increase top margin to make space for legend

# Create an empty plot
plot(x, y, pch = 19, col = "blue", xlim = c(x_min, x_max), ylim = c(y_min, y_max),
     xlab = "X", ylab = "Y", main = "Scatter Plot with Regression Line")

# Add regression line only inside the plot area
x_seq <- seq(x_min, x_max, length.out = 100)  # Generate x values within axis limits
y_seq <- 0.8 * x_seq + 1.2  # Compute corresponding y values
valid_indices <- (y_seq >= y_min) & (y_seq <= y_max)  # Ensure y-values stay inside the plot
lines(x_seq[valid_indices], y_seq[valid_indices], col = "red", lwd = 2)  # Draw regression line only inside the plot

# Add mean point
points(x_mean, y_mean, col = "green", pch = 19, cex = 1.5)

# Draw X and Y axes with arrows
arrows(x_min, 0, x_max, 0, col = "black", length = 0.1, lwd = 1.5) # X-axis arrow
arrows(0, y_min, 0, y_max, col = "black", length = 0.1, lwd = 1.5) # Y-axis arrow

# Move the legend outside the plot
par(xpd = TRUE)  # Allow legend outside plot area
legend("top", inset = -0.15,  # Moves the legend above the plot
       legend = c("Data Points", "Regression Line", "Mean (x̄, ȳ)"),
       pch = c(19, NA, 19),  # Symbol for points (Data & Mean), NA for line
       lty = c(NA, 1, NA),   # Line type (only for Regression Line)
       col = c("blue", "red", "green"),
       pt.cex = c(1, 1, 1.5),
       cex = 1,
       bty = "n")  # No legend border
```
## (f)
# Regression Analysis Results
![Rplot](https://github.com/user-attachments/assets/50e87af2-ae96-41f9-8e8d-3b2f6f1ca842)

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
$\sum{\hat{e_{i}}^2}$ =3.6
\
$\hat{\sigma}^2 = \hat{e_{i}}^2 / N-2 = 3.6 / 3 = 1.2$

## (j)
$\hat{Var}(b_2 | x)$ = $\hat{\sigma}^2$ / $\sum{(x_{i}-\bar{x})}^2$ = 1.2 / 10 = 0.12
\
$se(b_2)$ = $\sqrt{\hat{Var}(b_2 | x)}$ = 0.3464


