rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
commute5

# a.

model1 <- lm(time~depart+reds+trains, commute5)
smodel1 <- summary(model1)
smodel1

# b.

confint(model1,  ,level = 0.95)
vcov(model1)
