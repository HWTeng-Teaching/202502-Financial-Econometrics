library(POE5Rdata)
data(star)
?star
#a
ols = lm(readscore ~ small + aide + tchexper + boy+
         white_asian + freelunch, data = star)
summary(ols)

#b
#install.packages("plm")
library(plm)
wage_fixed = lm(readscore ~ small + aide + tchexper + boy+
                  white_asian + freelunch + factor(schid) - 1, data = star)
summary(wage_fixed)

#c
model_fe <- plm(readscore ~ small + aide + tchexper + boy + 
                white_asian + freelunch, data = star,
                model = "within",
                index = c("schid", "id"))
summary(model_fe)
ols_pooling <- plm(readscore ~ small + aide + tchexper + boy + 
                   white_asian + freelunch, data = star,
                   model = "pooling")
pFtest(model_fe, ols_pooling)

#d
star$sid <- star$id
newdata <- pdata.frame(star, index = c("schid", "sid"))

model_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = newdata, model = "random")
summary(model_d)

plmtest(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
        data = newdata,effect = "individual")
#p-value < 2.2e-16
#H0: no individual difference
#reject H0, there's indivual difference

#e
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch
               , data = newdata, model = "within")
hausman_test <- phtest(model_b, model_d)
hausman_test
#H0: no endogeneity
#p-value = 0.03184
fe_tab <- coef(summary(model_b))
re_tab <- coef(summary(model_d))
vars <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

for (v in vars) {
  diff    <- fe_tab[v, "Estimate"] - re_tab[v, "Estimate"]
  # Var(diff) = Var_FE - Var_RE  under RE exogeneity assumption
  se_diff <- sqrt(fe_tab[v, "Std. Error"]^2 - re_tab[v, "Std. Error"]^2)
  t_stat  <- diff / se_diff
  p_val   <- 2 * (1 - pnorm(abs(t_stat)))
  cat(sprintf("%-12s:  t = %5.2f,  p = %.3f\n", v, t_stat, p_val))
}
v <- "boy"
diff    <- fe_tab[v, "Estimate"] - re_tab[v, "Estimate"]
se_diff <- sqrt(fe_tab[v, "Std. Error"]^2 - re_tab[v, "Std. Error"]^2) #negative
t_stat  <- diff / se_diff
p_val   <- 2 * (1 - pnorm(abs(t_stat)))
cat(sprintf("%-12s:  t = %5.2f,  p = %.3f  (boy)\n", v, t_stat, p_val))

#f
# schid = school id, id = student id
pdata <- pdata.frame(star, index = c("schid","id"))
pdata$small_m       <- ave(pdata$small, pdata$schid)
pdata$aide_m        <- ave(pdata$aide, pdata$schid)
pdata$tchexper_m    <- ave(pdata$tchexper, pdata$schid)
pdata$boy_m         <- ave(pdata$boy, pdata$schid)
pdata$white_asian_m <- ave(pdata$white_asian, pdata$schid)
pdata$freelunch_m   <- ave(pdata$freelunch, pdata$schid)

# 確保無 NA 值
pdata_clean <- na.omit(pdata)

# Mundlak 模型
mundlak_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                       small_m + aide_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
                     data = pdata_clean,
                     model = "random")

summary(mundlak_model)
#p-value: < 2.22e-16
#H0: NO endogenous