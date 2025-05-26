#11.28
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("truffles")

install.packages("AER")
library(AER)       

#(b)Estimate the inverse demand and supply with 2SLS
## 2SLS – inverse demand (p ~ q + ps + di), instrument q with pf
demand.iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand.iv)

## 2SLS – inverse supply (p ~ q + pf), instrument q with ps & di
supply.iv <- ivreg(p ~ q + pf | pf + ps + di, data = truffles)
summary(supply.iv)

#(c)Price-elasticity of demand “at the means”
with(truffles, {
  Pbar <- mean(p);   Qbar <- mean(q)
  alpha2 <- 1 / coef(demand.iv)["q"]
  elas  <- alpha2 * Pbar / Qbar
  round(elas, 3)
})

#(d) Draw the two curves with the prescribed exogenous values
library(ggplot2)

DI0 <- 3.5; PS0 <- 22; PF0 <- 23

invDem <- function(q) coef(demand.iv)["(Intercept)"] +
  coef(demand.iv)["q"]  * q +
  coef(demand.iv)["ps"] * PS0 +
  coef(demand.iv)["di"] * DI0

invSup <- function(q) coef(supply.iv)["(Intercept)"] +
  coef(supply.iv)["q"]  * q +
  coef(supply.iv)["pf"] * PF0

curveData <- data.frame(
  q = seq(0, 35, length = 200)
) |> transform(
  Pd = invDem(q),
  Ps = invSup(q)
)

ggplot(curveData, aes(q)) +
  geom_line(aes(y = Pd), size = 1.1, linetype = "dashed",  ) +
  geom_line(aes(y = Ps), size = 1.1, color = "black") +
  labs(x = "Quantity (q)", y = "Price (p)",
       title = "Inverse Demand (dashed) and Supply (solid)") +
  theme_minimal()

#(e) Solve for the structural equilibrium and compare with the reduced-form prediction
# Intersection of the two straight lines
delta0 <- coef(demand.iv)["(Intercept)"] + coef(demand.iv)["ps"]*PS0 + coef(demand.iv)["di"]*DI0
gamma0 <- coef(supply.iv)["(Intercept)"] + coef(supply.iv)["pf"]*PF0
delta1 <- coef(demand.iv)["q"]
gamma1 <- coef(supply.iv)["q"]

q_star <- (delta0 - gamma0) / (gamma1 - delta1)
p_star <- invDem(q_star)          # or invSup(q_star)

c(p_star = round(p_star, 2), q_star = round(q_star, 2))

#(f) OLS estimates when simultaneity is ignored
demand.ols <- lm(p ~ q + ps + di, data = truffles)
supply.ols <- lm(p ~ q + pf,      data = truffles)

summary(demand.ols)
summary(supply.ols)