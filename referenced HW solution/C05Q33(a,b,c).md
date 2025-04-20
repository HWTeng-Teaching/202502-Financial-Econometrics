****學號：513717030 財金所碩專一 陳峻毅 Ian****


![圖片](https://github.com/user-attachments/assets/bece6810-2a6b-43da-b63c-2d52f2ba9f2b)

## a. Test whether each regression coefficient is significantly different from zero

According to the estimation results of the following regression model:

$$
\ln(WAGE) = \beta_1 + \beta_2 \cdot EDUC + \beta_3 \cdot EDUC^2 + \beta_4 \cdot EXPER + \beta_5 \cdot EXPER^2 + \beta_6 \cdot (EDUC \times EXPER) + e
$$

| variables           | values       | standard error     | $t$ value  | $p$ value         | siganificant |
|----------------|--------------|------------|---------|----------------|--------|
| (Intercept)     | 1.038        | 0.2757     | 3.764   | 0.0002         | ***    |
| EDUC            | 0.08954      | 0.03108    | 2.881   | 0.0040         | **     |
| EDUC²           | 0.001458     | 0.0009242  | 1.578   | 0.1149         |        |
| EXPER           | 0.04488      | 0.007297   | 6.150   | 1.06e-09       | ***    |
| EXPER²          | -0.000468    | 0.00007601 | -6.157  | 1.01e-09       | ***    |
| EDUC × EXPER    | -0.001010    | 0.0003791  | -2.665  | 0.0078         | **     |

| Symbol | Significance Level |
|--------|---------------------|
| `***`  | 1% level (very strong evidence) |
| `**`   | 5% level (strong evidence) |
| `*`    | 10% level (moderate evidence) |
| `.`    | 10%–20% level (weak evidence) |
| _(blank)_ | Not significant at 20% level |

At the 5% significance level, except for $\beta_3 (EDUC²)$, the coefficients of other variables are significantly different from 0.

---

## b. Marginal Effects of Education $\partial \ln(WAGE)/\partial EDUC$

$$
\frac{\partial \ln(WAGE)}{\partial EDUC} = \beta_2 + 2\beta_3 \cdot EDUC + \beta_6 \cdot EXPER
$$

The marginal effect of education varies with years of education and years of experience，and $\beta_3 > 0$ with $\beta_6 < 0$，means that the return on education decreases with experience.

。

---

## c. Estimate the marginal effect of education for each observation

Use the above formula to calculate the marginal effect of education for each data and draw a histogram. The results are summarized as follows:
d80-9a7a-78387011f9ff)

![5](https://github.com/user-attachments/assets/59861d46-b952-4106-8e48-e3d1e1194486)


- 5th percentile (5%)：0.0801
- Median (50%)：0.1084
- 95th percentile ：0.1336

This shows that the marginal effect of education on wages is concentrated around positive values.

---
