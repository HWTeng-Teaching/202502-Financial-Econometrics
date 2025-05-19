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

