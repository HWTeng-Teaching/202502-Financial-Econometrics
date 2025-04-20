url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/cps5_small.dat"
download.file(url, destfile = "cps5_small.dat")
cps5_small <- read.table("cps5_small.dat", header = FALSE)
colnames(cps5_small) <- c("BLACK","EDUC", "EXPER", "FAMINC","FEMALE","METRO","MIDWEST","SOUTH","WAGE","WEST")
head(cps5_small)  # 查看前幾行

# 建立模型
model <- lm(log(WAGE) ~ EDUC + I(EDUC^2) + EXPER + I(EXPER^2) + I(EDUC * EXPER), data = cps5_small)

# 查看回歸摘要
summary(model)


#(a) 各係數的顯著性檢定

summary(model)$coefficients

#• 看每個係數的 p-value 判斷是否顯著（小於 0.01、0.05 或 0.1）。

#(b) 教育的邊際效果：∂ln(WAGE)/∂EDUC

coefs <- coef(model)
cps5_small$ME_EDUC <- coefs["EDUC"] + 
  2 * coefs["I(EDUC^2)"] * cps5_small$EDUC + 
  coefs["I(EDUC * EXPER)"] * cps5_small$EXPER



#(c) 計算所有觀察值的教育邊際效果統計與直方圖

hist(cps5_small$ME_EDUC, main = "Marginal Effect of EDUC", xlab = "Marginal Effect")

quantile(cps5_small$ME_EDUC, probs = c(0.05, 0.5, 0.95))


#(d) 經驗的邊際效果：∂ln(WAGE)/∂EXPER

cps5_small$ME_EXPER <- coefs["EXPER"] + 
  2 * coefs["I(EXPER^2)"] * cps5_small$EXPER + 
  coefs["I(EDUC * EXPER)"] * cps5_small$EDUC


#(e) 計算經驗邊際效果統計與直方圖

hist(cps5_small$ME_EXPER, main = "Marginal Effect of EXPER", xlab = "Marginal Effect")

quantile(cps5_small$ME_EXPER, probs = c(0.05, 0.5, 0.95))



vcov_matrix2 <- vcov(model)
c_vec2 <- c(0, 1, 33, -10,-260,-152)
coef_column_vector <- matrix(coef(model), ncol = 1)
(betaf <- c_vec2 %*% coef_column_vector)
(sef2 <- sqrt(t(c_vec2) %*% vcov_matrix2 %*% c_vec2))


vcov_matrix2 <- vcov(model)
c_vec3 <- c(0, 1, 33, -10,-420,-144)
(betag <- c_vec3 %*% coef_column_vector)
(seg <- sqrt(t(c_vec3) %*% vcov_matrix2 %*% c_vec3))


vcov_matrix2 <- vcov(model)
c_vec4 <- c(0, 0, 0, 0, 12,-4)
(betah <- c_vec4 %*% coef_column_vector)
(seh <- sqrt(t(c_vec4) %*% vcov_matrix2 %*% c_vec4))




