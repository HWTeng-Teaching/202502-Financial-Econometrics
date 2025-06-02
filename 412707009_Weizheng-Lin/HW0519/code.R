Q06, Q20, Q17 ---

# ======================= Q06 =======================
rm(list = ls())

# Coefficient Comparison t-tests
coeff_data <- data.frame(
  EXPER  = c(0.0575, 0.0986, 0.0330, 0.0220),
  EXPER2 = c(-0.0012, -0.0023, 0.0011, 0.0007),
  SOUTH  = c(-0.3261, -0.2326, 0.1258, 0.0317),
  UNION  = c(0.0822, 0.1027, 0.0312, 0.0245)
)

t_EXPER  <- (coeff_data$EXPER[1]  - coeff_data$EXPER[2])  / sqrt(coeff_data$EXPER[3]^2  - coeff_data$EXPER[4]^2)
t_EXPER2 <- (coeff_data$EXPER2[1] - coeff_data$EXPER2[2]) / sqrt(coeff_data$EXPER2[3]^2 - coeff_data$EXPER2[4]^2)
t_SOUTH  <- (coeff_data$SOUTH[1]  - coeff_data$SOUTH[2])  / sqrt(coeff_data$SOUTH[3]^2  - coeff_data$SOUTH[4]^2)
t_UNION  <- (coeff_data$UNION[1]  - coeff_data$UNION[2])  / sqrt(coeff_data$UNION[3]^2  - coeff_data$UNION[4]^2)

cat("C15Q06 - Coefficient Comparison t-tests:\n")
cat("t(EXPER):", t_EXPER, "t(EXPER2):", t_EXPER2, "t(SOUTH):", t_SOUTH, "t(UNION):", t_UNION, 
    "\nCritical value: ±", qt(0.975, df = 4), "\n\n")

# ======================= Q20 =======================
library(POE5Rdata)
library(plm)
data("star")

# (a) OLS
mod <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
cat("C15Q20 - OLS Summary:\n")
print(summary(mod)$coefficients)

# (b) Fixed Effects
star$sid <- star$id
pdata <- pdata.frame(star, index = c("schid", "sid"))
femod <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata, model = "within")
cat("\nFixed Effects Summary:\n")
print(summary(femod)$coefficients)

# (c) F Test
cat("\nF Test (FE vs OLS):\n")
print(pFtest(femod, mod))

# (d) Random Effects + Pooling test
remod <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata, model = "random", random.method = "swar")
pomod <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = pdata, model = "pooling")
cat("\nPooling Test:\n")
print(plmtest(pomod, effect = "individual"))

# (e) FE vs RE coefficient comparison
diff_test <- function(f, r) {
  for (i in 1:6) {
    diff_se2 <- f[i, 2]^2 - r[i + 1, 2]^2
    if (diff_se2 > 0) {
      tval <- (f[i, 1] - r[i + 1, 1]) / sqrt(diff_se2)
      cat("t value of b", i + 1, ":", round(tval, 3), "\n")
    } else {
      cat("t value of b", i + 1, ": variance difference negative → cannot take sqrt\n")
    }
  }
  cat("Critical value: ±", round(qt(0.975, df = 6), 3), "\n")
}

cat("\nHausman-style Comparison (FE vs RE):\n")
diff_test(summary(femod)$coefficients, summary(remod)$coefficients)

# (f) Mundlak Regression
for (v in c("small", "aide", "tchexper", "boy", "white_asian", "freelunch")) {
  pdata[[paste0(v, "_avg")]] <- ave(pdata[[v]], pdata$schid)
}

mundlak_mod <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                     small_avg + aide_avg + tchexper_avg + boy_avg + white_asian_avg + freelunch_avg,
                   data = pdata, model = "random", random.method = "swar")
cat("\nMundlak Regression Summary:\n")
print(summary(mundlak_mod))

# ======================= Q17 =======================
data("liquor5")
pdata2 <- pdata.frame(liquor5, index = c("hh", "year"))

# (a) First-Difference
pdata2$LIQUORD <- diff(pdata2$liquor)
pdata2$INCOMED <- diff(pdata2$income)
model_fd <- lm(LIQUORD ~ INCOMED - 1, data = na.omit(pdata2))
cat("\nC15Q17 - First-Difference Model:\n")
print(summary(model_fd))

# (b) Random Effects
re_model2 <- plm(liquor ~ income, data = pdata2, model = "random", random.method = "swar")
cat("\nRandom Effects Model:\n")
print(summary(re_model2))

# (c) Pooling Test
pool_model2 <- plm(liquor ~ income, data = pdata2, model = "pooling")
cat("\nPooling Test (RE vs OLS):\n")
print(plmtest(pool_model2, effect = "individual"))

# (d) Mundlak Regression
income_avg <- aggregate(income ~ hh, data = liquor5, mean)
colnames(income_avg)[2] <- "INCOMEM"
pdata3 <- merge(pdata2, income_avg, by = "hh")
pdata3 <- pdata.frame(pdata3, index = c("hh", "year"))

mundlak2 <- plm(liquor ~ income + INCOMEM, data = pdata3, model = "random", random.method = "swar")
cat("\nMundlak Regression (C15Q17):\n")
print(summary(mundlak2))

