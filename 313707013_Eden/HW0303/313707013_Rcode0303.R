remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
#2.17
data("collegetown")
#a
data('collegetown') 
plot(collegetown$sqft, collegetown$price, pch=16, cex=0.5, 
     xlab = 'House Size, 100s', ylab = 'House Price, $1000')

#b
lin_model <- lm(price ~ sqft, data = collegetown)
summary(lin_model)
abline(lin_model, col = "red")

#c
collegetown$sqft_square <- collegetown$sqft^2
nonlin_model <- lm(price ~ sqft_square, data = collegetown)
summary(nonlin_model)

#d
quad_fun <- function(x){
    y=coef(nonlin_model)[1]+coef(nonlin_model)[2]*x^2
    return(y)
}
plot(collegetown$sqft,collegetown$price,pch=16,cex=0.6,xlab = 'House Size, 100s',ylab = 'House Price, $1000')
curve(quad_fun, col = "blue",add = TRUE,lwd=2)
m <- 40*coef(nonlin_model)[2]
tangline<- function(x){
  y=m*x+quad_fun(20)-m*20
}
curve(tangline, col = "red",add = TRUE,lwd=1.5)

#f
plot(collegetown$sqft,summary(lin_model)$resid,pch=16,cex=0.5,xlab = 'House Size, 100s', ylab = 'Residual ',main='Linear Model')
plot(collegetown$sqft,summary(nonlin_model)$resid,pch=16,cex=0.5,xlab = 'House Size, 100s', ylab = 'Residual',main='Quadratic Model')

#g
(resid1_square <- deviance(lin_model))
(resid2_square <- deviance(nonlin_model))


#2.25
data("cex5_small")
#a
str(cex5_small)
summary(cex5_small$foodaway)
hist(cex5_small$foodaway,breaks=30,main = 'Histogram of foodaway',xlab = 'FOODAWAY(Dollar per month per person)')

#b
# advanced 類別處理
cex5_small$advanced <- as.logical(cex5_small$advanced)
advanced_foodaway <- cex5_small$foodaway[cex5_small$advanced]
cat("Summary of advanced_foodaway:\n")
print(summary(advanced_foodaway))
cat("N (Advanced):", length(advanced_foodaway), "\n\n")
# college 類別處理
cex5_small$college <- as.logical(cex5_small$college)
college_foodaway <- cex5_small$foodaway[cex5_small$college]
cat("Summary of college_foodaway:\n")
print(summary(college_foodaway))
cat("N (College):", length(college_foodaway), "\n\n")
# 非 college 和 非 advanced 的處理
non_foodaway <- cex5_small$foodaway[!cex5_small$college & !cex5_small$advanced]
cat("Summary of non_foodaway:\n")
print(summary(non_foodaway))
cat("N (Non-college and Non-advanced):", length(non_foodaway), "\n")

#c
cex5_small$lnfoodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)
length(cex5_small$foodaway)
sum(!is.na(cex5_small$lnfoodaway))
summary(cex5_small$lnfoodaway)
hist_ln <- hist(cex5_small$lnfoodaway, 
                      main = "Histogram of ln(FOODAWAY)", 
                      xlab = "ln(FOODAWAY)", 
                      ylab = "Frequency", 
                      col = "lightgreen", 
                      border = "black",
                      freq = TRUE,  
                      breaks = 30)  


#d
linear_model_foodaway<- lm(lnfoodaway ~ income, data = cex5_small)
summary(linear_model_foodaway)


#e
plot(cex5_small$income,cex5_small$lnfoodaway,pch=16,cex=0.5,xlab = 'Income',ylab = 'ln(foodaway)')
abline(linear_model_foodaway, col = "red",lwd=2)

#f
resid_values <- summary(linear_model_foodaway)$resid
valid_idx <- !is.na(resid_values) & !is.na(cex5_small$income) # 確保兩個變數都沒有 NA
# 計算 resid_square，只使用完整資料
resid_square <- sum(resid_values[valid_idx])
# 繪圖，僅繪製完整資料
plot(
  cex5_small$income[valid_idx], 
  resid_values[valid_idx], 
  pch = 16, 
  cex = 0.5, 
  xlab = "Income",        # X 軸標籤
  ylab = "Residuals"      # Y 軸標籤
)

#2.28
#a
data("cps5_small")
#WAGE
summary(cps5_small$wage)
hist(cps5_small$wage,breaks = 30,xlab = "Hourly wage($)",main = 'Histogram of WAGE')
#EDUC
summary(cps5_small$educ)
hist(cps5_small$educ,breaks = 30,xlab = "Years of education",main = 'Histogram of EDUC')

#b
lin_model_wage <- lm(wage~educ, data = cps5_small)
summary(lin_model_wage)

#c
(resid_square_wage <- sum(summary(lin_model_wage)$resid))
plot(cps5_small$educ,(summary(lin_model_wage)$resid),pch=16,cex=0.5,xlab="Educ", ylab="Residuals")

#d
str(cps5_small)
#males
lin_model_males <- lm(wage~educ, data = cps5_small,subset=(female==0))
summary(lin_model_males)
#females
lin_model_females <- lm(wage~educ, data = cps5_small,subset=(female==1))
summary(lin_model_females)
#black
lin_model_black <- lm(wage~educ, data = cps5_small,subset=(black==1))
summary(lin_model_black)
#white
lin_model_white <- lm(wage~educ, data = cps5_small,subset=(black==0))
summary(lin_model_white)


#e
cps5_small$educ_square <- (cps5_small$educ)^2
quad_model_wage <- lm(wage~educ_square, data = cps5_small)
summary(quad_model_wage)

#f
plot(cps5_small$educ, cps5_small$wage, pch=16, cex=0.5, xlab = 'Years of education', ylab = 'Hourly wage($)', ylim = c(-50, 300), main = "Comparison Models")
abline(lin_model_wage, col = "green", lwd = 1.5, lty = 2)  # 使用 lty=2 来设置虚线
quad_fun_wage <- function(x){
  y = coef(quad_model_wage)[1] + coef(quad_model_wage)[2] * x^2
  return(y)
}
curve(quad_fun_wage, col = "blue", add = TRUE, lwd = 1.5)
legend("topleft", legend = c("Linear", "Quadratic"), 
       col = c("green", "blue"), lwd = 1.5, bty = "n")


