library(POE5Rdata)
library(plm)
data("star")
?star
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_a)

star_panel <- pdata.frame(star, index = c("schid", "id"))
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = star_panel, model = "within")
summary(model_b)

# Null: 所有學校固定效果為0 → 不需要 FE
pFtest(model_b, model_a)

# D
model_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = star_panel, model = "random")
summary(model_d)

# LM test for random effects
plmtest(model_d, effect = "individual", type = "bp")

#E
# Hausman 檢定：如果顯著 → 固定效果較合適
phtest(model_b, model_d)

coeftest(model_b)["boy",]
coeftest(model_d)["boy",]

# F
# 創造群組平均（學校平均值）
library(dplyr)

library(dplyr)

# 建立群組平均變數
star_panel <- star_panel %>%
  group_by(schid) %>%
  mutate(across(c(small, aide, tchexper, boy, white_asian, freelunch),
                list(mean = ~mean(.x, na.rm = TRUE)),
                .names = "{.col}_mean")) %>%
  ungroup()

names(star_panel)

# 加入群組平均後的 Mundlak 模型
model_f <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch +
                 small_mean + aide_mean + tchexper_mean + boy_mean +
                 white_asian_mean + freelunch_mean,
               data = star_panel, model = "random")
summary(model_f)

