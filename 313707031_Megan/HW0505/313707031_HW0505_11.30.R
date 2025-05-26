#11.30
install.packages("systemfit")
install.packages("broom")
install.packages("dplyr")
library(systemfit)
library(AER)
library(lmtest)
library(car)
library(sandwich)
library(broom)
library(dplyr)
library(POE5Rdata)
data("klein")
summary(klein)

# rename to symbols used here
klein <- transform(KleinI,
                   CN  = consump,           # personal consumption
                   I   = invest,                # private investment
                   TIME= year - 1931,           # trend starting at 0
                   K_1 = capitalLag,            # lagged capital
                   P_1 = corpProfLag,           # lagged profits  (instrument only)
                   G   = govExp,  Wg = govWage, T = taxes,  X_1 = gnpLag)
klein <- na.omit(klein[
  , c("CN","I","TIME","K_1","P_1","G","Wg","T","X_1")])

#(a) OLS estimates of the investment function 
ols_inv <- lm(i ~ p + plag + klag, data = klein)
cat("\n(a)  OLS estimates of 11.18:\n")
summary(ols_inv)
print(tidy(ols_inv), n = Inf) 

#(b)Reduced-form
rf_P <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = klein)
summary(rf_P)

joint_test <- linearHypothesis(
  rf_P,
  c("g = 0", "w2 = 0", "tx = 0", "time = 0", "elag = 0")
)
cat("\nJoint F-test (g, w2, tx, time, elag):\n")
print(joint_test, digits = 3)

F_stat <- joint_test$F[2]       
df1    <- joint_test$Df[2]      
df2    <- joint_test$Res.Df[2]  
F_crit <- qf(0.95, df1, df2)   

cat("F statistic =", round(F_stat, 3), "\n")
cat("Critical F(5,13;0.95) =", round(F_crit, 3), "\n")

df <- klein |>
  as.data.frame() |>
  select(year, cn, i, p, plag, klag, e, elag, w2, g, tx, time) |>
  na.omit()                     
nrow(df)  

rf_P  <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = df)
df$phat <- fitted(rf_P)
df$vhat <- resid(rf_P)
df$phat

#(c)Hausman test
hausman <- lm(i ~ p + plag + klag + vhat, data = df)
cat("\n(c)  Hausman test (t-stat of vhat):\n")
print(coeftest(hausman, vcov. = vcovHC(hausman, type = "HC1"))["vhat", ])
summary(hausman)

#(d)2SLS estimates of the investment equation
iv_inv <- ivreg(i ~ p + plag + klag |
                  g + w2 + tx + time + plag + klag + elag,
                data = df)
cat("\n(d) 2SLS estimates of 11.18:\n")
print(tidy(iv_inv), n = Inf)
summary(iv_inv)

compare_slopes <- bind_rows(
  tidy(ols_inv) |> mutate(model = "OLS"),
  tidy(iv_inv)  |> mutate(model = "2SLS")
) |>
  select(model, term, estimate, std.error, statistic, p.value)

cat("\nOLS vs 2SLS coefficients:\n")
print(compare_slopes, n = Inf)

#(e)Second-stage “plug-in” regression
stage2 <- lm(i ~ phat + plag + klag, data = df)
cat("\n(e)  Second-stage OLS (I_t on p̂):\n")
summary(stage2)

#(f)Sargan test of over-identifying restrictions
e2hat <- resid(iv_inv)
sargan_aux <- lm(e2hat ~ g + w2 + tx + time + elag + plag + klag, data = df)
R2  <- summary(sargan_aux)$r.squared
TR2 <- nrow(df) * R2
df_sargan <- 4                      # L-B = 5-1 = 4
crit95 <- qchisq(0.95, df_sargan)

cat("\n(f)  Sargan test:\n")
cat("   TR^2  =", round(TR2, 3), "\n")
cat("   χ²_0.95(df=4) =", round(crit95, 3), "\n")
if (TR2 < crit95) {
  cat("   → Fail to reject H0 : surplus instruments appear valid.\n")
} else {
  cat("   → Reject H0 : at least one surplus instrument may be invalid.\n")
}
