url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/commute5.dat"
download.file(url, destfile = "commute5.dat")
data <- read.table("commute5.dat", header = FALSE)
colnames(data) <- c("TIME","DEPART", "REDS", "TRAINS")
head(data)  # 查看前幾行
model <- lm(TIME ~ DEPART + REDS + TRAINS, data = data)
summary(model)

t <- qt(0.95,df=245)
print(t)

# 計算變異數-共變異數矩陣
vcov_matrix <- vcov(model)

# 顯示變異數矩陣
print(vcov_matrix)
# 建立模型
model <- lm(TIME ~ DEPART + REDS + TRAINS, data = data)

# 提取變異數-共變異數矩陣
v <- vcov(model)

# 計算 Var(β4 - 3β3)
var_beta4_minus_3beta3 <- v["TRAINS", "TRAINS"] + 
  9 * v["REDS", "REDS"] - 
  6 * v["REDS", "TRAINS"]

# 顯示結果
print(var_beta4_minus_3beta3)

# 假設已建立模型
model <- lm(TIME ~ DEPART + REDS + TRAINS, data = data)

# 取得變異數-共變異數矩陣
vcov_matrix <- vcov(model)

# 建立係數向量 a = [1, 30, 6, 1]
a <- c(1, 30, 6, 1)

# 計算變異數 Var(b1 + 30*b2 + 6*b3 + b4)
var_combination <- t(a) %*% vcov_matrix %*% a

# 顯示結果
print(var_combination)