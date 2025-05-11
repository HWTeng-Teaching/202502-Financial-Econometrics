#### This homework is written by 卓伯呈(313707012)
# CH11 Q1
Our aim is to estimate the parameters of the simultaneous equations model 

$$
y_1 =α_1 y_2 +e_1
$$

$$
y_2 =α_2 y_1 +β_1 x_1 +β_2 x_2 +e_2
$$

We assume that $x_1$ and $x_2$ are exogenous and uncorrelated with the error terms $e_1$ and $e_2$.

## **Question(a):**

#### a. Solve the two structural equations for the reduced-form equation for $y_2$, that is, $$y_2  =π_1 x_1  +π_2 x_2  +v_2$$ . Express the reduced-form parameters in terms of the structural parameters and the reduced-form error in terms of the structural parameters and $e_1$ and $e_2$.Show that $y_2$ is correlated with $e_1$.

### ANS:
$$
y_2 = \alpha_2(\alpha_1 y_2 + e_1) + \beta_1 x_1 + \beta_2 x_2 + e_2 = \alpha_2 \alpha_1 y_2 + \alpha_2 e_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

$$
(1 - \alpha_2 \alpha_1)y_2 = \alpha_2 e_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

$$
y_2 = \frac{\beta_1}{1 - \alpha_2 \alpha_1} x_1 + \frac{\beta_2}{1 - \alpha_2 \alpha_1} x_2 + \frac{\alpha_2 e_1 + e_2}{1 - \alpha_2 \alpha_1}
$$

$$
\Rightarrow y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
$$

$$
\text{cov}(y_2, e_1) = \text{cov}(\pi_1 x_1 + \pi_2 x_2 + v_2, e_1)
$$

$$
= \text{cov}(v_2, e_1) = \text{cov}\left( \frac{\alpha_2 e_1 + e_2}{1 - \alpha_2 \alpha_1}, e_1 \right)= \frac{\alpha_2}{1 - \alpha_2 \alpha_1} \text{Var}(e_1) \Rightarrow \text{cov}(y_2, e_1) \neq 0 \quad \text{if } \alpha_2 \neq 0
$$

## **Question(b):**
#### b. Which equation parameters are consistently estimated using OLS? Explain.
### ANS:
$$
y_1 = \alpha_1 x_1  + e_1, \quad \text{we know that } \text{cov}(y_2, e_1) \neq 0 \Rightarrow \text{endogeneity problem} \Rightarrow \text{OLS is biased and inconsistent}
$$
$$
y_2 =α_2 y_1 +β_1 x_1 +β_2 x_2 +e_2, \text{cov}(y_1, e_1) = \text{cov}(\alpha_1(\pi_1 x_1 + \pi_2 x_2 + v_2), e_1) = \text{cov}(\beta_1 x_1 + e_1, e_1)
$$
$$
= \beta_1 \text{cov}(x_1, e_1) + \text{var}(e_1) \neq 0
$$
$$
\Rightarrow \text{endogeneity problem} \Rightarrow \text{OLS is biased and inconsistent}
$$
## **Question(c):**
#### c. Which parameters are “identified,” in the simultaneous equations sense? Explain your reasoning.
### ANS:
