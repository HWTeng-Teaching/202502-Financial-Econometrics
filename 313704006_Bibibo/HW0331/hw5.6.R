b = c(2, 3, -1)
cov <- c(3, -2, 1, -2, 4, 0, 1, 0, 3)
cov_matrix <- matrix(cov, nrow = 3, ncol = 3, byrow = TRUE)  # 按行填充
print(cov_matrix)
N = 63
K = 3
df = N-K

#a. β2=0
t = b[2]/(cov_matrix[2, 2]^0.5)
tc = qt(0.025, df, lower.tail = FALSE)
t > tc #non-reject H0

#b. β1+2β2=5
coefficient = c(1, 2, 0)
estV = sum(coefficient*b)
varV = t(coefficient) %*% cov_matrix %*% coefficient
stdV = varV^0.5
t = (estV - 5)/stdV
t > tc #non-reject H0

#c. β1−β2+β3=4
coefficient = c(1, -1, 1)
estV = sum(coefficient*b)
varV = t(coefficient) %*% cov_matrix %*% coefficient
stdV = varV^0.5
t = (estV - 4)/stdV
abs(t) > tc #non-reject H0

