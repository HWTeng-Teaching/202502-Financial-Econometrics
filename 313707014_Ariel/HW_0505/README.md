### f.

$$
\frac{1}{N} \sum x_{i1}(y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
$$

$$
\frac{1}{N} \sum x_{i2}(y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
$$

已知：

$$
\sum x_{i1}^2 = 1,\quad \sum x_{i2}^2 = 1,\quad \sum x_{i1}x_{i2} = 0
$$

$$
\sum x_{i1} y_{1i} = 2,\quad \sum x_{i1} y_{2i} = 3,\quad \sum x_{i2} y_{1i} = 3,\quad \sum x_{i2} y_{2i} = 4
$$

推導第 1 個方程：

$$
\sum x_{i1} y_2 - \pi_1 \sum x_{i1}^2 - \pi_2 \sum x_{i1} x_{i2} = 0
$$

$$
3 - \pi_1 \cdot 1 - \pi_2 \cdot 0 = 0
$$

$$
\hat{\pi}_1 = 3
$$

推導第 2 個方程：

$$
\sum x_{i2} y_2 - \pi_1 \sum x_{i2} x_{i1} - \pi_2 \sum x_{i2}^2 = 0
$$

$$
4 - 3 \cdot 0 - \pi_2 \cdot 1 = 0
$$

$$
\hat{\pi}_2 = 4
$$


---

### g.

$$
y_1 = \alpha_1 y_2 + e_1
$$

工具變數為：

$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2
$$  

且 $\hat{y}_2$ 和 $e_1$ 無相關。



$$
E[(\hat{\pi}_1 x_1 + \hat{\pi}_2 x_2)(y_1- \alpha_1 y_2 ) \mid x] = 0
$$

$$
\frac{1}{N} \sum \begin{pmatrix} \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2 \end{pmatrix} (y_1- \alpha_1 y_2 ) = 0
$$

$$
\Rightarrow \sum (\hat{y}_2) (y_1- \alpha_1 y_2 ) = 0
$$

$$
\Rightarrow \sum \hat{y}_2 y_1 - \alpha_1 \sum \hat{y}_2 y_2 = 0
$$

$$
\Rightarrow \hat{\alpha_1} = \frac{\sum \hat{y}_2 y_1}{\sum\hat{y}_2 y_2}
$$

$$
\Rightarrow \frac{\sum y_1 (\hat{\pi}_1 x_1 + \hat{\pi}_2 x_2)}{\sum y_2 (\hat{\pi}_1 x_1 + \hat{\pi}_2 x_2)}
$$

$$
\Rightarrow \frac{\hat{\pi}_1 \sum y_1 x_1 + \hat{\pi}_2 \sum y_1 x_2}{ \hat{\pi}_1 \sum y_2 x_1 + \hat{\pi}_2 \sum y_2 x_2}
$$

代入數值：

$$
= \frac{3 \times 2 + 4 \times 3}{3 \times 3 + 4 \times 4} = \frac{18}{25}
$$

---

### h. 證明兩者相同（g 與 moment condition 的結果）

$$
\hat{\alpha_1} = \frac{\sum \hat{y}_2 y_1}{\sum \hat{y}_2 y_2} 
= \frac{\sum \hat{y}_2 y_1}{\sum \hat{y}_2^2}
$$

這與方法 g 的結果（使用 OLS 回歸）所得到的估計值相同。


根據數學 moment condition 推導：

$$
\hat{y}_2 = y_2 - v_i \Rightarrow y_2 = \hat{y}_2 + v_2
$$

$$
\sum \hat{y}_2 y_2 = \sum \hat{y}_2 (\hat{y}_2 + v_2) 
= \sum \hat{y}_2^2 + \sum \hat{y}_2 v_2
$$

由於 $\hat{y}_2$ 和 $v_2$ 不相關，

$$
\sum \hat{y}_2 v_2 = 0
$$

所以：

$$
\sum \hat{y}_2^2 + \sum \hat{y}_2 v_2 = \sum \hat{y}_2^2 + 0
$$

因此得證。



