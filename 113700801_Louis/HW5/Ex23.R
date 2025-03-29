> library(POE5Rdata)
> data("cocaine")
> modele <- lm(price ~ quant + qual + trend, data = cocaine)
> summary(modele)


> r_squared <- summary(modele)$r.squared
> r_squared

> coef_quant <- summary(modele)$coefficients["quant", "Estimate"]
> p_value_quant <- summary(modele)$coefficients["quant", "Pr(>|t|)"]
> coef_quant
> p_value_quant
> if (p_value_quant < 0.05 && coef_quant < 0) {
+   cat("On rejette H₀ : il existe une relation négative significative entre QUANT et PRICE.")
+ } else {
+   cat("On ne rejette pas H₀ : il n'y a pas de relation négative significative entre QUANT et PRICE.")
+ }

> coef_qual <- summary(modele)$coefficients["qual", "Estimate"]
> p_value_qual <- summary(modele)$coefficients["qual", "Pr(>|t|)"]
> coef_qual
> p_value_qual
> if (p_value_qual < 0.05 && coef_qual > 0) {
+   cat("On rejette H₀ : il existe un premium significatif pour une meilleure qualité de cocaïne.")
+ } else {
+   cat("On ne rejette pas H₀ : il n'y a pas de premium significatif pour une meilleure qualité de cocaïne.")
+ }


> cocaine_sorted <- cocaine %>%
+   arrange(trend)
> price_changes <- diff(cocaine_sorted$price)
> avg_annual_change <- mean(price_changes)
> avg_annual_change
