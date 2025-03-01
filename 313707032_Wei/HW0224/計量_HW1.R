# 下載數據並儲存為本地檔案
download.file("https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata", 
              destfile = "capm5.rdata", mode = "wb")

# 載入數據
load("capm5.rdata")

# 查看數據結構
str(capm5)
summary(capm5)
head(capm5)
# 建立空的結果列表
results <- list()
firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")  # 使用正確的變數名稱

# 迴歸分析 (CAPM)
for (firm in firms) {
    results[[firm]] <- lm((capm5[[firm]] - capm5$riskfree) ~ (capm5$mkt - capm5$riskfree), data=capm5)
}

# 顯示回歸結果
lapply(results, summary)


#2.16(b)
# 提取 β 值
beta_values <- sapply(results, function(model) coef(model)[2])

# 顯示 β 值
beta_values

# 確保 beta_values 有正確的名稱
names(beta_values) <- firms  

# 找出最具攻擊性和最具防禦性的公司
most_aggressive <- names(beta_values)[which.max(beta_values)]
most_defensive <- names(beta_values)[which.min(beta_values)]

cat("最具攻擊性的公司:", most_aggressive, "\n")
cat("最具防禦性的公司:", most_defensive, "\n")



#2.16(c)
# 繪製 Microsoft 的市場超額報酬與個股超額報酬關係
plot(capm5$mkt - capm5$riskfree, capm5$msft - capm5$riskfree, 
     xlab="市場超額報酬 (Market Excess Return)", 
     ylab="Microsoft 超額報酬 (Microsoft Excess Return)", 
     main="Microsoft 的 CAPM 回歸模型",
     pch=16, col="blue")

# 添加回歸線
abline(results$msft, col="red", lwd=2)
# 提取自變數 (市場超額報酬) 和 因變數 (Microsoft 超額報酬)
market_excess <- capm5$mkt - capm5$riskfree
msft_excess <- capm5$msft - capm5$riskfree

# 重新進行回歸
msft_model <- lm(msft_excess ~ market_excess)

# 繪製散點圖
plot(market_excess, msft_excess, 
     xlab="市場超額報酬", 
     ylab="Microsoft 超額報酬", 
     main="Microsoft 的 CAPM 回歸模型",
     pch=16, col="blue")

# 手動繪製回歸線
abline(msft_model, col="red", lwd=2)



#2.16(d)
# 1️⃣ 載入數據並檢查 ---------------------------------------------------
if (!exists("capm5")) {
    load("capm5.rdata")  # 確保數據已載入
}

# 檢查數據結構
str(capm5)
summary(capm5)
head(capm5)
names(capm5)  # 確保變數名稱正確

# 設定公司名稱，與數據欄位匹配
firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# 2️⃣ 估計 CAPM 模型（含截距 \alpha）-------------------------------------
results <- list()
for (firm in firms) {
    results[[firm]] <- lm((capm5[[firm]] - capm5$riskfree) ~ (capm5$mkt - capm5$riskfree), data=capm5)
}

# 提取 β 值
beta_values <- sapply(results, function(model) coef(model)[2])

# 顯示 β 值
print(beta_values)

# 3️⃣ 估計 CAPM 模型（假設 \alpha = 0）-----------------------------------
results_no_intercept <- list()
for (firm in firms) {
    results_no_intercept[[firm]] <- lm((capm5[[firm]] - capm5$riskfree) ~ 0 + (capm5$mkt - capm5$riskfree), data=capm5)
}

# 提取 β 值（無截距）
beta_values_no_intercept <- sapply(results_no_intercept, function(model) coef(model)[1])

# 4️⃣ 比較 β 值 ---------------------------------------------------------
comparison <- data.frame(
    Company = firms,
    Beta_With_Alpha = beta_values,
    Beta_No_Alpha = beta_values_no_intercept
)

# 顯示比較表
print(comparison)

# 5️⃣ 繪製 β 值比較圖 ---------------------------------------------------
# 確保 Company 為字符格式
comparison$Company <- as.character(comparison$Company)

# 轉換數據為數字矩陣
comparison_matrix <- as.matrix(comparison[,2:3])

# 處理 NA 值（若有）
comparison_matrix[is.na(comparison_matrix)] <- 0

# 確保為數字格式
comparison_matrix <- apply(comparison_matrix, 2, as.numeric)

# 轉置矩陣，使不同 β 值顯示在同一組
comparison_matrix <- t(comparison_matrix)

# 設定條形圖顏色
colors <- c("skyblue", "orange")

# 繪製條形圖
barplot(comparison_matrix, beside=TRUE, col=colors,
        names.arg=comparison$Company, main="CAPM β 值比較", 
        ylim=c(0, max(comparison_matrix) + 0.2), 
        xlab="公司", ylab="β 值")

# 添加圖例
legend("topleft", legend=c("With Alpha", "No Alpha"), 
       fill=colors, bty="n")

# 6️⃣ 顯示回歸結果（檢查 \alpha 是否顯著）--------------------------------
lapply(results, summary)


# 設定公司名稱，與數據欄位匹配
firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# 2️⃣ 估計 CAPM 模型（含 \alpha）---------------------------------------------------
results <- list()
for (firm in firms) {
    results[[firm]] <- lm((capm5[[firm]] - capm5$riskfree) ~ (capm5$mkt - capm5$riskfree), data=capm5)
}

# 提取 β 值
beta_values <- sapply(results, function(model) coef(model)[2])

# 顯示 β 值
print(beta_values)

# 3️⃣ 估計 CAPM 模型（假設 \alpha = 0）---------------------------------------------------
results_no_intercept <- list()
for (firm in firms) {
    results_no_intercept[[firm]] <- lm((capm5[[firm]] - capm5$riskfree) ~ 0 + (capm5$mkt - capm5$riskfree), data=capm5)
}

# 提取 β 值（無截距）
beta_values_no_intercept <- sapply(results_no_intercept, function(model) coef(model)[1])

# 4️⃣ 比較 β 值 ---------------------------------------------------
comparison <- data.frame(
    Company = firms,
    Beta_With_Alpha = beta_values,
    Beta_No_Alpha = beta_values_no_intercept
)

# 顯示比較表
print(comparison)

# 5️⃣ 繪製 β 值比較圖 ---------------------------------------------------
# 確保 Company 為字符格式
comparison$Company <- as.character(comparison$Company)

# 轉換數據為矩陣
comparison_matrix <- as.matrix(comparison[,2:3])

# 轉置矩陣，使不同 β 值顯示在同一組
comparison_matrix <- t(comparison_matrix)

# 繪製條形圖
barplot(comparison_matrix, beside=TRUE, col=c("skyblue", "orange"),
        names.arg=comparison$Company, main="CAPM β 值比較", 
        ylim=c(0, max(comparison_matrix) + 0.2))

# 添加圖例
legend("topleft", legend=c("With Alpha", "No Alpha"), 
       fill=c("skyblue", "orange"), bty="n")

# 6️⃣ 顯示回歸結果（檢查 \alpha 是否顯著）---------------------------------------------------
lapply(results, summary)
# 1️⃣ 確保 Company 為字符格式
comparison$Company <- as.character(comparison$Company)

# 2️⃣ 轉換數據為數字矩陣
comparison_matrix <- as.matrix(comparison[,2:3])

# 3️⃣ 處理 NA 值（若有）
comparison_matrix[is.na(comparison_matrix)] <- 0

# 4️⃣ 轉換數據類型，確保為數字格式
comparison_matrix <- apply(comparison_matrix, 2, as.numeric)

# 5️⃣ 轉置矩陣，使不同 β 值顯示在同一組
comparison_matrix <- t(comparison_matrix)

# 6️⃣ 設定條形圖顏色
colors <- c("skyblue", "orange")

# 7️⃣ 強制刷新繪圖視窗，確保新圖表顯示
dev.off()

# 8️⃣ 繪製條形圖
barplot(comparison_matrix, beside=TRUE, col=colors,
        names.arg=comparison$Company, main="CAPM β 值比較", 
        ylim=c(0, max(comparison_matrix) + 0.2), 
        xlab="公司", ylab="β 值")

# 9️⃣ 添加圖例
legend("topleft", legend=c("With Alpha", "No Alpha"), 
       fill=colors, bty="n")



