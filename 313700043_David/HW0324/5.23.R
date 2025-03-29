rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
cocaine

#5.23 b

model1 <- lm(price~quant+qual+trend, cocaine)
summary(model1)

