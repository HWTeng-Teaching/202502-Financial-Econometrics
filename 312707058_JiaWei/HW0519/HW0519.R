#-----------------C15Q06-----------------
#(f)
rm(list = ls())
df = data.frame(
  EXPER =  c(0.0575,0.0986,0.0330,0.0220),
  EXPER2 = c(-0.0012,-0.0023,0.0011,0.0007),
  SOUTH =  c(-0.3261,-0.2326,0.1258,0.0317),
  UNION =  c(0.0822,0.1027,0.0312,0.0245)
)
t_EXPER = (df$EXPER[1]-df$EXPER[2])/sqrt(df$EXPER[3]^2-df$EXPER[4]^2)
t_EXPER2 = (df$EXPER2[1]-df$EXPER2[2])/sqrt(df$EXPER2[3]^2-df$EXPER2[4]^2)
t_SOUTH = (df$SOUTH[1]-df$SOUTH[2])/sqrt(df$SOUTH[3]^2-df$SOUTH[4]^2)
t_UNION = (df$UNION[1]-df$UNION[2])/sqrt(df$UNION[3]^2-df$UNION[4]^2)
cat(' t(EXPER)  t(EXPER2)  t(SOUTH)  t(UNION)  crtical value\n',
    t_EXPER,t_EXPER2,t_SOUTH,t_UNION, "  ±",qt(1-0.05/2,5-1))

#-----------------C15Q20-----------------
rm(list = ls())
library(POE5Rdata)
#install.packages('plm')
library(plm)
data("star")
#(a)
mod <- lm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch , data = star)
sum = summary(mod)
sum$coefficients

#(b)
star$sid <- star$id
pdata <- pdata.frame(star, index = c("schid", "sid"))
femod <- plm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch,
                data = pdata, model = "within")
fesum = summary(femod)
fesum$coefficients

#(c)
pFtest(femod, mod)

#(d)
remod <- plm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch,
             data = pdata,radom.method='swar', model = "random")
resum = summary(remod)
resum$coefficients
pomod <- plm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch,
             data = pdata, model = "pooling")
plmtest(pomod, effect = "individual")

#(e)
f = fesum$coefficients
r = resum$coefficients
for (i in 1:6)
{
  if (f[i,2]^2-r[i+1,2]^2 > 0)
  {cat("t value of b",i+1,":", (f[i,1]-r[i+1,1])/sqrt(f[i,2]^2-r[i+1,2]^2),"\n")}
  else
  {cat("t value of b",i+1,":", "se(bFE)^2 − se(bRE)^2 =",f[i,2]^2-r[i+1,2]^2,"→ cannot be square-rooted\n")}
  if (i == 6)
    {cat('crtical value  : ±', qt(1-0.05/2,7-1),"\n")}
}

#(f)

pdata$small_avg       <- ave(pdata$small,       pdata$schid)
pdata$aide_avg        <- ave(pdata$aide,        pdata$schid)
pdata$tchexper_avg    <- ave(pdata$tchexper,    pdata$schid)
pdata$boy_avg         <- ave(pdata$boy,         pdata$schid)
pdata$white_asian_avg <- ave(pdata$white_asian, pdata$schid)
pdata$freelunch_avg   <- ave(pdata$freelunch,   pdata$schid)
pdata_clean <- na.omit(pdata)

mundlak_mod <- plm(readscore ~ small+aide+tchexper+boy+white_asian+freelunch
                   +small_avg+aide_avg+tchexper_avg+boy_avg+white_asian_avg+freelunch_avg,
                     data = pdata_clean, radom.method='swar', model = "random")
summary(mundlak_mod)

S#-----------------C15Q17-----------------
rm(list = ls())
library(POE5Rdata)
#ls("package:POE5Rdata")
data("liquor5")
#install.packages("plyr")
library(plyr)

library(dplyr)
library(plm)

#(a)
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

pdata$LIQUORD <- diff(pdata$liquor)
pdata$INCOMED <- diff(pdata$income)

pdata_clean <- na.omit(pdata)
model <- lm(LIQUORD ~ INCOMED -1, data = pdata_clean)
summary(model)
confint(model, level = 0.95)

#(b)
remod <- plm(liquor ~ income, data = pdata,radom.method='swar', model = "random")
resum = summary(remod)
resum$coefficients

confint(remod, level = 0.95)

#(c)
pomod <- plm(liquor ~ income, data = pdata, model = "pooling")
plmtest(pomod, effect = "individual")

#(d)
INCOMEM <- aggregate(list(INCOMEM = liquor5$income), by = list(hh = liquor5$hh), FUN = mean)

pdata_avg <- merge(pdata, INCOMEM, by = "hh")
pdata_avg <- pdata.frame(pdata_avg, index = c("hh", "year"))

mundlak_mod <- plm(liquor ~ income+INCOMEM,
                   data = pdata_avg, radom.method='swar', model = "random")
summary(mundlak_mod)
