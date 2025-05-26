# 
star <- read.csv("star.csv")

model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

summary(model_a)

library(plm)

# 設定為 panel data frame（個體為學校，時間為學生 ID）
pdata <- pdata.frame(star, index = c("schid", "id"))

# 固定效果模型
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = pdata, model = "within", effect = "individual")

# 顯示結果
summary(model_b)





# pooled OLS 模型（無固定效果）
model_pooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pdata, model = "pooling")

# F 檢定（比較固定效果模型與 pooled 模型）
pFtest(model_b, model_pooled)



#(d)
# 載入套件
library(plm)
library(lmtest)

# 讀檔並轉成 panel data
star <- read.csv("star.csv")
pdata <- pdata.frame(star, index = c("schid", "id"))

# (a) 的 pooling 模型：作為 LM 檢定的基準
mod_pool <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "pooling"
)

# (d) 隨機效果模型（Swamy–Arora 方法）
mod_re <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "random",
  random.method = "swar"
)

# 列出 Random Effects 的估計結果
summary(mod_re)

# Breusch–Pagan LM test：檢定是否需要納入個體（學校）隨機效果
plmtest(mod_pool, effect = "individual", type = "bp")



#(e)

library(plm)

# 先跑 FE（fixed effects）與 RE（random effects）模型
mod_fe <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "within"
)
mod_re <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "random",
  random.method = "swar"
)

# 把 coef 和 se 抓出來
fe <- summary(mod_fe)$coefficients     # matrix: Estimate, Std. Error, ...
re <- summary(mod_re)$coefficients

# 要檢定的變數名稱
vars <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")

# 計算 t 值
t_stats <- sapply(vars, function(v) {
  b_fe  <- fe[v, "Estimate"]
  b_re  <- re[v, "Estimate"]
  se_fe <- fe[v, "Std. Error"]
  se_re <- re[v, "Std. Error"]
  (b_fe - b_re) / sqrt(se_fe^2 - se_re^2)
})

# 顯示結果
t_stats


#(f)
library(plm)
library(lmtest)
library(sandwich)

pdata <- pdata.frame(star, index = c("schid", "id"))

fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata, model = "within")

re_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata, model = "random", random.method = "swar")

haus <- phtest(fe_b, re_d)

tv_raw <- c("small", "aide", "tchexper", "freelunch")

varying <- tv_raw[
  sapply(tv_raw, function(v){
    all(tapply(star[[v]], star$schid, var, na.rm = TRUE) > 0, na.rm = TRUE)
  })
]

sch_avg <- star %>% 
  group_by(schid) %>% 
  summarise(across(c(small, aide, tchexper,
                     boy, white_asian, freelunch),
                   mean, .names = "{.col}_avg"))

star_m <- left_join(star, sch_avg, by = "schid") %>%
  pdata.frame(index = c("schid","id"))

form_m <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
  small_avg + aide_avg + tchexper_avg +
  boy_avg + white_asian_avg + freelunch_avg

ols_m   <- lm(form_m, data = star_m)
vc_cl   <- vcovCL(ols_m, cluster = ~ schid)

linearHypothesis(ols_m,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = vc_cl)


