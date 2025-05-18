url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"
file_path <- "star.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(star)

# a
ols_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = star)
summary(ols_a)

#b
pdata <- pdata.frame(star, index = c("schid","id"))

fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata,
            model = "within")
summary(fe_b)

#c
pool_c <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = pdata,
              model = "pooling")

pFtest(fe_b, pool_c)

names(star)
