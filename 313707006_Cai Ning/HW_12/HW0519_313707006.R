#15.17
library(POE5Rdata)
data("liquor5")

#(a)
# 按照家庭與年份排序
liquor5 <- liquor5[order(liquor5$hh, liquor5$year), ]

# 建立第一差分
liquor5$liquord <- ave(liquor5$liquor, liquor5$hh, FUN = function(x) c(NA, diff(x)))
liquor5$incomed <- ave(liquor5$income, liquor5$hh, FUN = function(x) c(NA, diff(x)))

# 拿掉 NA
df <- na.omit(liquor5[, c("liquord", "incomed")])

# 無截距回歸
model <- lm(liquord ~ incomed + 0, data = df)

# 顯示結果與 95% 信賴區間
summary(model)
confint(model)

#(b)
library(plm)

# 設定 panel 資料格式
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

# 建立隨機效果模型
re_model <- plm(liquor ~ income, data = pdata, model = "random")

# 顯示估計結果
summary(re_model)

# 計算 95% 信賴區間
confint(re_model)

#(c)
# 先建一個 pooled OLS 模型（用 plm 而不是 lm）
pooled_model <- plm(liquor ~ income, data = pdata, model = "pooling")
# 再用 LM test 檢定是否有 random effects
plmtest(pooled_model, effect = "individual", type = "bp")

#(d)
# Step 1：計算個體平均收入 INCOMEM
liquor5$incomem <- ave(liquor5$income, liquor5$hh)

# Step 2：轉成 panel 格式
pdata2 <- pdata.frame(liquor5, index = c("hh", "year"))

# Step 3：估計帶 INCOMEM 的隨機效果模型（Mundlak 補充）
mundlak_model <- plm(liquor ~ income + incomem, data = pdata2, model = "random")

# Step 4：顯示結果與檢定
summary(mundlak_model)


#15.20
library(POE5Rdata)
data("star")

#(a)
model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model)

#(b)
# 改掉 id 這個變數名稱
star$stid <- star$id
star$id <- NULL  # 刪除原本的 id 欄位（避免混淆）

# 指定學校為個體（做學校 fixed effects）
pdata <- pdata.frame(star, index = c("schid", "stid"))

# 跑學校固定效果模型
fe_school <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata,
                 model = "within",
                 effect = "individual")
summary(fe_school)

#(c)
# 無固定效果的 OLS 模型
ols_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                 data = pdata,
                 model = "pooling")

# 固定效果模型（學校固定效果）
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata,
                model = "within",
                effect = "individual")

# F 檢定
pFtest(fe_model, ols_model)

#(d)
# 學校隨機效果模型
re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata,
                model = "random",
                effect = "individual")
summary(re_model)

# 用 OLS 模型做 LM 檢定（Breusch-Pagan test）
plmtest(ols_model, effect = "individual", type = "bp")

#(e)
# 比較 FE 與 RE 模型的係數差異（Hausman test）
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

#(f)
library(sandwich)
library(car)


pdata <- pdata.frame(star, index = c("schid", "stid"))

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

# 建立學校平均變數
sch_avg <- star %>% 
  group_by(schid) %>% 
  summarise(across(c(small, aide, tchexper,
                     boy, white_asian, freelunch),
                   mean, .names = "{.col}_avg"))

# 合併學校平均值並轉成 panel 資料
star_m <- left_join(star, sch_avg, by = "schid") %>%
  pdata.frame(index = c("schid","stid"))

# 建立模型公式
form_m <- readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
  small_avg + aide_avg + tchexper_avg +
  boy_avg + white_asian_avg + freelunch_avg

# 套用 OLS 與 cluster-robust 標準誤
ols_m   <- lm(form_m, data = star_m)
vc_cl   <- vcovCL(ols_m, cluster = ~ schid)

# 檢定學校平均變數是否共同為 0
linearHypothesis(ols_m,
                 c("small_avg = 0", "aide_avg = 0", "tchexper_avg = 0",
                   "boy_avg = 0", "white_asian_avg = 0", "freelunch_avg = 0"),
                 vcov. = vc_cl)
