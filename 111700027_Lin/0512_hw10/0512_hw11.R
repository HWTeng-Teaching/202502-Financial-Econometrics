#15.17
#(A)
install.packages(POE5Rdata)
library(POE5Rdata)
data("liquor5")

install.packages(dplyr)
library(dplyr)

liquor_diff = liquor5 %>%
  group_by(hh) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    d_liquor = liquor - dplyr::lag(liquor),
    d_income = income - dplyr::lag(income)
  ) %>%
  filter(!is.na(d_liquor) & !is.na(d_income)) %>%
  ungroup()

reg_result = lm(d_liquor ~ d_income + 0, data = liquor_diff)

summary(reg_result)

confint(reg_result, "d_income", level = 0.95)

#15.20
#(A)
data("star")

fit_model = lm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data = star
)

summary(fit_model)

#(B)(C)
library(dplyr)

star = star %>%
  mutate(student_id = id)

fe_model = lm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch + factor(schid),
  data = star
)

cat("=== 固定效果模型（含學校 dummy）摘要 ===\n")
summary(fe_model)

pooled_model = lm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data = star
)

cat("\n=== Pooled OLS 模型摘要 ===\n")
summary(pooled_model)

cat("\n=== F 檢定：是否需要加入固定效果 ===\n")
anova(pooled_model, fe_model)
