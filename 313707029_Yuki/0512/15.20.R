#── 清除環境 ─────────────────────────────
rm(list = ls())
#── 讀套件 ────────────────────────────────
library(POE5Rdata)
library(plm)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(nlme)


#── 載入資料，並將 id 改成 year ───────────
data("star")
star <- star %>% rename(year = id)


#── (a) 普通 OLS：READSCORE ~ SMALL + AIDE + TCHEXPER + BOY + WHITE_ASIAN + FREELUNCH
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = star)
summary(model_a)

# 然后再跑 (b) 学校固定效果
pdata <- pdata.frame(star, index = c("schid", "year"))
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data  = pdata,
                model = "within")
summary(fe_model)


# (c) 固定效果顯著性 F‐檢定
pFtest(fe_model, model_a)

# (d) 随机效果 + LM-test
# (d1) 隨機效果
re_school <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data   = pdata,
                 model  = "random",
                 effect = "individual")
summary(re_school)

# (d2) LM 檢定
plmtest(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
        data   = pdata,
        effect = "individual",
        type   = "bp")

#── (e) FE vs. RE 個別係數差異 t‐檢定 ─────────────────
# 取係數與變異數
fe_coef  <- coef(fe_model)
re_coef  <- coef(re_school)
fe_vcov  <- vcov(fe_model)
re_vcov  <- vcov(re_school)
vars     <- c("small","aide","tchexper","white_asian","freelunch","boy")

test_res <- t(sapply(vars, function(v){
  delta    <- fe_coef[v] - re_coef[v]
  se_d     <- sqrt(fe_vcov[v,v] + re_vcov[v,v])
  t_val    <- delta / se_d
  p_val    <- 2 * (1 - pnorm(abs(t_val)))
  c(Estimate = delta, StdError = se_d,
    t_value  = t_val,  p_value = p_val)
}))
as.data.frame(test_res)


#── (f) Mundlak—Pooling 版本 ─────────────────
# 1. 計算每校自變數平均
school_means <- star %>%
  group_by(schid) %>%
  summarise(
    mean_small       = mean(small,       na.rm = TRUE),
    mean_aide        = mean(aide,        na.rm = TRUE),
    mean_tchexper    = mean(tchexper,    na.rm = TRUE),
    mean_boy         = mean(boy,         na.rm = TRUE),
    mean_white_asian = mean(white_asian, na.rm = TRUE),
    mean_freelunch   = mean(freelunch,   na.rm = TRUE)
  )

# 2. 合併校平均
df2 <- star %>%
  left_join(school_means, by = "schid")

# 3. 保留分析欄位並剔除 NA
df2_clean <- df2 %>%
  select(readscore, small, aide, tchexper, boy, white_asian, freelunch,
         mean_small, mean_aide, mean_tchexper, mean_boy, mean_white_asian, mean_freelunch,
         schid) %>%
  na.omit()

# 4. 建立基準隨機截距模型（不含校平均），以 ML 估計
base_ml <- lme(
  fixed  = readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  random = ~1 | schid,
  data   = df2_clean,
  method = "ML"
)

# 5. 建立 Mundlak 增強模型（含校平均），以 ML 估計
mundlak_ml <- lme(
  fixed  = readscore ~ small + aide + tchexper + boy + white_asian + freelunch
  + mean_small + mean_aide + mean_tchexper
  + mean_boy + mean_white_asian + mean_freelunch,
  random = ~1 | schid,
  data   = df2_clean,
  method = "ML"
)

# 6. Likelihood‐ratio test：檢定所有 mean_* 係數 = 0
lr_res <- anova(base_ml, mundlak_ml)
print(lr_res)
