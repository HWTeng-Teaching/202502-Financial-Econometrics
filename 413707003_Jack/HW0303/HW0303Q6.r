# a

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
load(url(url))


summary(cps5_small$wage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.94   13.00   19.30   23.64   29.80  221.10 

summary(cps5_small$educ)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    12.0    14.0    14.2    16.0    21.0 

hist(cps5_small$wage, main="Histogram of WAGE", xlab="wage", col="blue")
hist(cps5_small$educ, main="Histogram of EDUC", xlab="educ", col="green")


# b

linear_model <- lm(wage ~ educ, data=cps5_small)

summary(linear_model)


# beta_1 = -10.4  (educ = 0 -> wage = -10.4)
# beta_2 = 2.3968  (educ increase 1-> wage increase 2.3968)
