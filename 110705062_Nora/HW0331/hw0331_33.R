url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
file_path <- "cps5_small.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cps5_small)

model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model)

cps5_small$marginal_educ <- coef(model)["educ"] +
  2 * coef(model)["I(educ^2)"] * cps5_small$educ +
  coef(model)["I(educ * exper)"] * cps5_small$exper

hist(cps5_small$marginal_educ,
     breaks = 40,
     main = "Marginal Effect of EDUC",
     xlab = "∂ln(WAGE)/∂EDUC",
     col = "lightblue",
     border = "white")


quantile(cps5_small$marginal_educ, probs = c(0.05, 0.5, 0.95))
cps5_small$marginal_exper <- coef(model)["exper"] +
  2 * coef(model)["I(exper^2)"] * cps5_small$exper +
  coef(model)["I(educ * exper)"] * cps5_small$educ

hist(cps5_small$marginal_exper,
     breaks = 40,
     main = "Marginal Effect of EXPER",
     xlab = "∂ln(WAGE)/∂EXPER",
     col = "lightgreen",
     border = "white")

quantile(cps5_small$marginal_exper, probs = c(0.05, 0.5, 0.95))
