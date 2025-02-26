The capital asset pricing model (CAPM) is an important model in the field of finance. It explains variations in the net return on a stock as a function of the rate of return on a portfolio that includes all traded stocks, which is called the market portfolio. Specifically, the CAPM says that the expected return on a stock is the risk-free rate plus a risk premium that depends on how sensitive the stock is to movements in the market. That sensitivity is called the beta. The CAPM also says that the risk premium, \( r_{jm} - r_f \), is proportional to the market risk premium. The resulting portfolio is that

$$
r_{jm} - r_f = \beta_j (r_m - r_f)
$$

where \( r_j \) and \( r_f \) are the returns on the \( j \)th stock and the risk-free rate, respectively, and \( \beta_j \) is the \( j \)th security’s “beta” value. A stock’s beta is important in portfolio analysis because a stock with a beta greater than one is considered “aggressive,” meaning it has above-average risk and above-average expected returns. Conversely, a stock with a beta less than one is considered “defensive” since its return is less sensitive to movements in the market. In practice, we do not observe the expected return on the stock. Instead, we assume that the average returns in the data estimate a stock’s beta before purchasing it. The CAPM model shown in equation (1) is a theoretical expression in this case. The “econometric model” is obtained by including an intercept in the equation (even though theory says it should be zero) and an error term:

$$
r_{jm} = \alpha_j + \beta_j (r_m - r_f) + e_j
$$

(a) Explain why the econometric model above is a simple regression model like those discussed in the text.

(b) In the data file capm5 are data on the monthly returns for five stocks (GE, IBM, Ford, Microsoft, Disney), and the excess returns on the market portfolio (MKT), and the return on the three-month T-bill as the risk-free asset (RISKFREE). The 180 observations cover January 1998 to December 2012. The variable \( r_{jm} \) is the stock’s return minus the risk-free rate. The variable \( r_m - r_f \) is the market’s return minus the risk-free rate.

(c) Regress \( r_{jm} \) on \( (r_m - r_f) \) for each stock and discuss the size of the estimated betas. Which stocks appear to be aggressive? Which are defensive?

(d) Estimate the intercepts in the regressions. Are they all close to zero? Are they statistically different from zero?

(e) Obtain the slope estimate for the Microsoft stock. How does it compare to the slope for the other stocks?

(f) Finally, put the slope estimate under the assumption that \( \alpha_j = 0 \). Do the estimates of the betas change much?
