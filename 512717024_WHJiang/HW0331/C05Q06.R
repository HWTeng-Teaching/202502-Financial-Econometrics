# 設定已知的最小平方法係數與共變異數矩陣
b <- c(2, 3, -1)
names(b) <- c("b1", "b2", "b3")

V <- matrix(c(3, -2, 1,
              -2, 4, 0,
               1, 0, 3), nrow = 3, byrow = TRUE)
rownames(V) <- colnames(V) <- c("b1", "b2", "b3")

# 樣本大小與自由度 (樣本數 - 估計參數數量)
n <- 63
df <- n - length(b)  # 60

# (a) 檢定 H0: β₂ = 0
# b2 = 3, Var(b2) = 4 => SE = sqrt(4)=2
t_a <- (b["b2"] - 0) / sqrt(V["b2", "b2"])
p_a <- 2 * pt(-abs(t_a), df = df)  # 雙尾檢定
cat("Test (a): H0: β₂ = 0\n")
cat("t =", t_a, ", p-value =", p_a, "\n\n")

# (b) 檢定 H0: β₁ + 2β₂ = 5
# 設線性組合 L = [1, 2, 0]
L_b <- c(1, 2, 0)
est_b <- sum(L_b * b)  # 2 + 2*3 = 8
var_b <- as.numeric(t(L_b) %*% V %*% L_b)  # 計算變異數
se_b <- sqrt(var_b)
t_b <- (est_b - 5) / se_b
p_b <- 2 * pt(-abs(t_b), df = df)
cat("Test (b): H0: β₁ + 2β₂ = 5\n")
cat("Estimated value =", est_b, ", SE =", se_b, "\n")
cat("t =", t_b, ", p-value =", p_b, "\n\n")

# (c) 檢定 H0: β₁ - β₂ + β₃ = 4
# 設線性組合 L = [1, -1, 1]
L_c <- c(1, -1, 1)
est_c <- sum(L_c * b)  # 2 - 3 + (-1) = -2
var_c <- as.numeric(t(L_c) %*% V %*% L_c)  # 計算變異數
se_c <- sqrt(var_c)
t_c <- (est_c - 4) / se_c
p_c <- 2 * pt(-abs(t_c), df = df)
cat("Test (c): H0: β₁ - β₂ + β₃ = 4\n")
cat("Estimated value =", est_c, ", SE =", se_c, "\n")
cat("t =", t_c, ", p-value =", p_c, "\n")
