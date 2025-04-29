############################################################
# R 程式碼：Wooldridge 10.18（mroz 資料）
# ----------------------------------------------------------
# 套件：AER 內含 ivreg()；lmtest、sandwich 提供穩健檢定
############################################################

# 0. 套件與資料 ------------------------------------------------------------
if (!require("AER"))        install.packages("AER")
if (!require("lmtest"))     install.packages("lmtest")
if (!require("sandwich"))   install.packages("sandwich")
if (!require("dplyr"))      install.packages("dplyr")

library(AER)        # ivreg(), waldtest(), overid()
library(lmtest)     # coeftest(), waldtest()
library(sandwich)   # vcovHC()
library(dplyr)

# 讀取資料（請把路徑改成你的檔案位置）
mroz  <- read.csv("mroz.csv")

# 只保留參與勞動市場的 428 位觀測值 ------------------------
mroz_lfp <- filter(mroz, lfp == 1)

# 1. 變數衍生 ---------------------------------------------------------------
mroz_lfp <- mroz_lfp %>% 
  mutate(
    MOTHERCOLL = as.integer(mothereduc > 12),
    FATHERCOLL = as.integer(fathereduc > 12),
    exper2     = exper^2,
    lwage      = log(wage)
  )

# 2. (a) 父母具有部分大學教育之比例 ---------------------------
perc_mother <- mean(mroz_lfp$MOTHERCOLL) * 100
perc_father <- mean(mroz_lfp$FATHERCOLL) * 100
cat(sprintf("母親 ≧1 年大學教育：%.2f%%\n", perc_mother))
cat(sprintf("父親 ≧1 年大學教育：%.2f%%\n", perc_father))

# 3. (b) 相關係數 -----------------------------------------------------------
cor_mat <- with(mroz_lfp, cor(cbind(educ, MOTHERCOLL, FATHERCOLL)))
print(round(cor_mat, 3))

# 4. (c) 單工具（MOTHERCOLL）IV/2SLS --------------------------------------
iv1 <- ivreg(
  lwage ~ educ + exper + exper2 |    # 被解釋 + 內生 + 外生
    MOTHERCOLL + exper + exper2,     # 工具 + 外生
  data = mroz_lfp
)
summary(iv1, diagnostics = TRUE)     # diagnostics 顯示弱工具檢定等

# 95% CI for βeduc
beta1  <- coef(iv1)["educ"]
se1    <- sqrt(vcovHC(iv1, type = "HC1")["educ","educ"])
ci95_1 <- beta1 + c(-1.96, 1.96)*se1
cat("95% CI (single IV) for β_EDUC:", round(ci95_1, 3), "\n")

# 第一階段 F ---------------------------------------------------------------
fs1 <- summary(iv1$stage1)["educ", ]  # stage1 物件的摘要
F_first <- fs1["F"]
cat("First-stage F (MOTHERCOLL):", round(F_first, 2), "\n")

# 5. (e) 兩工具（MOTHERCOLL, FATHERCOLL）-----------------------------------
iv2 <- ivreg(
  lwage ~ educ + exper + exper2 |
    MOTHERCOLL + FATHERCOLL + exper + exper2,
  data = mroz_lfp
)
summary(iv2, diagnostics = TRUE)

# 95% CI for βeduc
beta2  <- coef(iv2)["educ"]
se2    <- sqrt(vcovHC(iv2, type = "HC1")["educ","educ"])
ci95_2 <- beta2 + c(-1.96, 1.96)*se2
cat("95% CI (two IVs) for β_EDUC:", round(ci95_2, 3), "\n")

# 6. (f) 聯合弱工具檢定 (first-stage F) -----------------------------------
# 先抓第一階段迴歸，再用 waldtest() 檢定兩個 IV
fs2 <- iv2$stage1          # 第一階段 lm 物件
waldtest(fs2, . ~ . - MOTHERCOLL - FATHERCOLL)  # H0: 兩者係數=0

# 7. (g) 過度識別檢定 (Sargan–Hansen J) -----------------------------------
library(ivmodel)           # 若未安裝，可先安裝：install.packages("ivmodel")
overid(iv2)                # 或 AER::overid()

############################################################
#   完成！依序執行即可得到題目 (a)–(g) 所需之所有統計量
############################################################
