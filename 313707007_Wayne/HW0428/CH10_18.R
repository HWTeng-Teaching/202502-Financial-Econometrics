#A
# 設定工作目錄（請依你自己的路徑調整）
setwd("C:/Users/USER/Desktop/碩一下資料/計量經濟學/POE")

# 讀取 CSV 檔案（例如名稱為 mroz.csv）
mroz_csv <- read.csv("poe5rdata/mroz.csv")

# 建立虛擬變數：母親與父親是否教育年數 > 12
mroz_csv <- mroz_csv %>%
  mutate(
    mothercoll = ifelse(mothereduc > 12, 1, 0),
    fathercoll = ifelse(fathereduc > 12, 1, 0)
  )

# 計算百分比
mother_coll_pct <- mean(mroz_csv$mothercoll) * 100
father_coll_pct <- mean(mroz_csv$fathercoll) * 100

# 輸出結果
cat("母親有部分大學教育的比例：", round(mother_coll_pct, 2), "%\n")
cat("父親有部分大學教育的比例：", round(father_coll_pct, 2), "%\n")


#B
# 載入必要套件
library(dplyr)
library(wooldridge)

# 載入資料
mroz_csv <- read.csv("poe5rdata/mroz.csv")

# 讀取資料（如果尚未執行）
# mroz <- read.csv("你的路徑/mroz.csv")

# 篩選出參與勞動市場的女性
mroz_active <- subset(mroz, inlf == 1)

# 建立 MOTHERCOLL 和 FATHERCOLL 虛擬變數
mroz_active$MOTHERCOLL <- ifelse(mroz_active$motheduc > 12, 1, 0)
mroz_active$FATHERCOLL <- ifelse(mroz_active$fatheduc > 12, 1, 0)

# 計算教育與這兩個工具變數間的相關係數
cor_matrix <- cor(mroz_active[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print(cor_matrix)



#c
# 載入必要套件
library(AER)

# 假設 mroz_active 已建立好，包含 MOTHERCOLL 等變數
# 並已篩選 inlf == 1 的樣本

# 工具變數迴歸：使用 MOTHERCOLL 作為 EDUC 的工具變數
iv_model <- ivreg(wage ~ educ + exper + expersq | MOTHERCOLL + exper + expersq, data = mroz_active)

# 顯示結果摘要
summary(iv_model)

# 計算 EDUC 係數的 95% 信賴區間
confint(iv_model, level = 0.95)

# 估計第一階段模型：educ 是被預測的變數
first_stage <- lm(educ ~ MOTHERCOLL + exper + expersq, data = mroz_active)

# 查看第一階段回歸係數
summary(first_stage)


#d
# 第一階段：OLS 迴歸
first_stage <- lm(educ ~ MOTHERCOLL + exper + expersq, data = mroz_active)

# 顯示摘要
summary(first_stage)


#e
# 載入 AER 套件（如果尚未執行過）
library(AER)

# 使用兩個工具變數：MOTHERCOLL 和 FATHERCOLL
iv_model_2 <- ivreg(wage ~ educ + exper + expersq | MOTHERCOLL + FATHERCOLL + exper + expersq, data = mroz_active)

# 查看結果
summary(iv_model_2)

# 95% 信賴區間
confint(iv_model_2, level = 0.95)


#f
# 建立經驗平方變數
mroz_active$expersq <- mroz_active$exper^2

# 第一階段模型：以 educ 為被解釋變數，工具變數為 MOTHERCOLL 與 FATHERCOLL
first_stage_2iv <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + expersq, data = mroz_active)

# 查看回歸結果摘要
summary(first_stage_2iv)

# 若還沒安裝 car 套件，請先安裝
# install.packages("car")
library(car)

# 檢定 MOTHERCOLL 和 FATHERCOLL 同時為 0
linearHypothesis(first_stage_2iv, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#i
# 先載入必要套件
library(AER)

# 確保你已經建立過 iv_model_2（含 MOTHERCOLL + FATHERCOLL）
# 這是 part (e) 的 IV 模型

# 加上 diagnostics = TRUE 查看工具變數診斷
summary(iv_model_2, diagnostics = TRUE)
