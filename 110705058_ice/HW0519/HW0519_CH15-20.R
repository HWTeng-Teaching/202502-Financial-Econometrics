url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"
file_path <- "star.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(star)

# a
ols_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = star)
summary(ols_a)

#b
pdata <- pdata.frame(star, index = c("schid","id"))

fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata,
            model = "within")
summary(fe_b)

#c
pool_c <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = pdata,
              model = "pooling")

pFtest(fe_b, pool_c)

names(star)

#d
re_d <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "random"
)
summary(re_d)

plmtest(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data   = pdata,
  type   = "bp",
  effect = "individual"
)

#e
haus <- phtest(fe_b, re_d)
haus
qchisq(0.95,df=6)
fe_tab <- coef(summary(fe_b))
re_tab <- coef(summary(re_d))

vars <- c("small", "aide", "tchexper", "white_asian", "freelunch")

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
se_diff <- sqrt(fe_tab[v, "Std. Error"]^2 - re_tab[v, "Std. Error"]^2)
t_stat  <- diff / se_diff
p_val   <- 2 * (1 - pnorm(abs(t_stat)))
cat(sprintf("%-12s:  t = %5.2f,  p = %.3f  (boy)\n", v, t_stat, p_val))

#f
library(plm)
library(lmtest)
library(sandwich)




pdata$small_m       <- ave(pdata$small,       pdata$schid)
pdata$aide_m        <- ave(pdata$aide,        pdata$schid)
pdata$tchexper_m    <- ave(pdata$tchexper,    pdata$schid)
pdata$boy_m         <- ave(pdata$boy,         pdata$schid)
pdata$white_asian_m <- ave(pdata$white_asian, pdata$schid)
pdata$freelunch_m   <- ave(pdata$freelunch,   pdata$schid)

# 確保無 NA 值
pdata_clean <- na.omit(pdata)

# Mundlak 模型
mundlak_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                       small_m + aide_m + tchexper_m + boy_m + white_asian_m + freelunch_m,
                     data = pdata_clean,
                     model = "random")

summary(mundlak_model)