library(POE5Rdata)
library(lmtest)
library(carData)
library(car)
data(cps5)
model1 = lm(log(wage)~educ+exper+I(exper^2)+female+black+metro+south+midwest+west, data = cps5)

#a
modela = lm(log(wage)~educ+exper+I(exper^2)+metro, data = cps5)
male = cps5[cps5$female==0, ]
female = cps5[cps5$female==1, ]
male_lm = lm(log(wage)~educ+exper+I(exper^2)+metro, data = male)
female_lm = lm(log(wage)~educ+exper+I(exper^2)+metro, data = female)

male_df = male_lm$df.residual
female_df = female_lm$df.residual
f = summary(male_lm)$sigma^2 / summary(female_lm)$sigma^2
f_low = qf(0.025, male_df, female_df)
f_up = qf(0.975, male_df, female_df)
(f > f_low)*(f < f_up) #1 non-reject H0
#non-reject H0

#b
model1 = lm(log(wage)~educ+exper+I(exper^2)+female+black+metro+south+midwest+west, data = cps5)
ressq = resid(model1)^2
modres2 = lm(ressq~educ+exper+I(exper^2)+female+black+metro+south+midwest+west, data = cps5)
n = nobs(modres2) #9799
S = nobs(modres2) - df.residual(modres2) #10
rsqres = summary(modres2)$r.squared
chisq = n * rsqres
pval = 1-pchisq(chisq, S - 1) #0 reject H0

#c
modres3 = lm(ressq~educ+I(educ^2)+exper+I(exper^2)+I(exper^4)+female+I(female^2)+black+I(black^2)+metro+I(metro^2)+south+I(south^2)+midwest+I(midwest^2)+west+I(west^2)+
            educ*exper+educ*I(exper^2)+I(exper^3), data = cps5)
n = nobs(modres3) #9799
S = nobs(modres3) - df.residual(modres3) #15
rsqres = summary(modres3)$r.squared
chisq = n * rsqres
pval = 1-pchisq(chisq, S - 1) #0 reject H0

#d
summary(model1)
library(lmtest)
library(carData)
explanable = c("intercept","educ","exper","I(exper^2)","female","\black","metro","south","midwest","west")
cov1 = hccm(model1, type = "hc1")
wage.hci = coeftest(model1, vcov. = cov1)
for(i in 1:nrow(wage.hci)){
  if(summary(model1)$coefficients[i, 2] < wage.hci[i, 2]){
    print(paste(explanable[i],"has wider interval after robust"))
  }
  else{
    print(paste(explanable[i],"has narrower interval after robust"))
  }
}

#e
ehatsq = resid(model1)^2
sighatsq = lm(ehatsq~metro+exper, data = cps5)
vari = fitted(sighatsq)
cps5.fgls = lm(log(wage)~educ+exper+I(exper^2)+female+black+metro+south+midwest+west, weights = 1/vari, data = cps5)
summary(cps5.fgls)

#f
cov2 = hccm(cps5.fgls, type = "hc1")
cps5.fgls.robust = coeftest(cps5.fgls, vcov. = cov2)
cps5.fgls.robust

#g
plot(cps5$metro, resid(model1))
plot(cps5$metro, resid(cps5.fgls))
plot(cps5$exper, resid(model1))
plot(cps5$exper, resid(cps5.fgls))
#plot(cps5$exper^2, resid(model1))
#plot(cps5$exper^2, resid(cps5.fgls))
#plot(cps5$educ, resid(model1))
#plot(cps5$educ, resid(cps5.fgls))
#plot(cps5$female, resid(model1))
#plot(cps5$female, resid(cps5.fgls))
#plot(cps5$black, resid(model1))
#plot(cps5$black, resid(cps5.fgls))
