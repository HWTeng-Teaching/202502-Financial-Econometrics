############################################################################
# STAR Kindergarten  –  Exercise 15.20
# 完整腳本：重現 POE5  Table XR15.20  (OLS / RE / FE / Mundlak)
############################################################################
options(digits = 10)

##── 0. 套件 ────────────────────────────────────────────────────────────────
pkgs <- c("tidyverse", "plm", "lmtest", "car", "lme4", "modelsummary")
to_add <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_add)) install.packages(to_add, quiet = TRUE)
suppressPackageStartupMessages(lapply(pkgs, library, character.only = TRUE))

##── 1. 載入資料並移除 NA（教材樣本 N = 5 766）───────────────────────────
load(url(
  "https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"))

vars  <- c("readscore","small","aide","tchexper","boy",
           "white_asian","freelunch","schid","id")
starK <- star |> select(all_of(vars)) |> na.omit()
stopifnot(nrow(starK) == 5766)

##── 2. OLS ----------------------------------------------------------------
ols <- lm(readscore ~ small + aide + tchexper + boy +
            white_asian + freelunch, data = starK)

##── 3. 建立 panel 物件 ----------------------------------------------------
pdata <- pdata.frame(starK, index = c("schid","id"), drop.index = TRUE)

##── 4. 固定效果
#      4-a  within 估計（給檢定用）
fe_plm <- plm(readscore ~ small + aide + tchexper + boy +
                white_asian + freelunch,
              data = pdata, model = "within")
#      4-b  LSDV (sum contrasts) → 截距 = 全體平均
old_ct <- options(contrasts = c("contr.sum","contr.poly"))
fe_lsdv <- lm(readscore ~ small + aide + tchexper + boy +
                white_asian + freelunch + factor(schid),
              data = starK)
options(old_ct)

##── 5. 隨機效果  (Swamy–Arora) -------------------------------------------
re <- plm(readscore ~ small + aide + tchexper + boy +
            white_asian + freelunch,
          data = pdata, model = "random", effect = "individual",
          random.method = "swar")

##── 6. Mundlak  (lme4 隨機截距) -------------------------------------------
starM <- starK %>%
  group_by(schid) %>%
  mutate(across(c(small,aide,tchexper,boy,white_asian,freelunch),
                mean, .names = "{.col}M")) %>%
  ungroup()

mund <- lmer(readscore ~ small + aide + tchexper + boy +
               white_asian + freelunch +
               smallM + aideM + tchexperM + boyM +
               white_asianM + freelunchM +
               (1 | schid),
             data = starM, REML = FALSE)

##── 7. 產生 Table XR15.20  (四位小數) ------------------------------------
fmt4 <- function(x) format(round(as.numeric(x), 4), nsmall = 4)

modelsummary(
  list("OLS"     = ols,
       "RE"      = re,
       "FE"      = fe_lsdv,
       "Mundlak" = mund),
  statistic = "({std.error})",
  fmt       = fmt4,
  stars     = c('*'=.10,'**'=.05,'***'=.01),
  coef_map  = c("(Intercept)"="C",
                "small"="SMALL","aide"="AIDE","tchexper"="TCHEXPER",
                "boy"="BOY","white_asian"="WHITE_ASIAN","freelunch"="FREELUNCH",
                "smallM"="SMALLM","aideM"="AIDEM","tchexperM"="TCHEXPERM",
                "boyM"="BOYM","white_asianM"="WHITE_ASIANM","freelunchM"="FREELUNCHM"),
  coef_omit = "factor\\(schid\\)",          # 隱藏 78 個 school dummies
  gof_omit  = "IC|Log|Adj|R2|F|Sigma",
  add_rows  = data.frame(rowname = "N",
                         OLS=5766, RE=5766, FE=5766, Mundlak=5766),
  title     = "Table XR15.20  (Kindergarten READSCORE, N = 5 766)"
)






pFtest(fe_plm, plm(readscore ~ small + aide + tchexper + boy +
                     white_asian + freelunch, data=pdata, model="pooling"))
plmtest(re, type="bp")
phtest(fe_plm, re)
car::linearHypothesis(mund,
                      c("smallM=0","aideM=0","tchexperM=0","boyM=0","white_asianM=0","freelunchM=0"),
                      test="Chisq")

