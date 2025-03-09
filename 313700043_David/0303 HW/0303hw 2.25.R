rm(list = ls())  
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
cex5_small

#2.18 a

#全部數據
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 5) +
  labs(title = "Histogram of Foodaway",
       x = "Foodaway Spending",
       y = "Frequency") +
  theme_minimal()
#只到380
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 5) +
  scale_x_continuous(limits = c(0, 380), breaks = seq(0, 380, 20))+
  labs(title = "Histogram of Foodaway",
       x = "Foodaway Spending",
       y = "Frequency") +
  theme_minimal()

mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)  
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)  
percentiles <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)  

print(paste0("Mean: ", round(mean_foodaway, 2)))
print(paste0("Median: ", round(median_foodaway, 2)))
print(paste0("25th Percentile (Q1): ", round(percentiles[1], 2)))
print(paste0("75th Percentile (Q3): ", round(percentiles[2], 2)))

#2.25 b

advanced_stats <- cex5_small %>%
  filter(advanced == 1) %>%
  summarise(
    Mean_foodaway_advanced = mean(foodaway, na.rm = TRUE),
    Median_foodaway_advanced = median(foodaway, na.rm = TRUE)
  )

college_stats <- cex5_small %>%
  filter(college == 1) %>%
  summarise(
    Mean_foodaway_college = mean(foodaway, na.rm = TRUE),
    Median_foodaway_college = median(foodaway, na.rm = TRUE)
  )

no_degree_stats <- cex5_small %>%
  filter(advanced == 0 & college == 0) %>%
  summarise(
    Mean_foodaway_others = mean(foodaway, na.rm = TRUE),
    Median_foodaway_others = median(foodaway, na.rm = TRUE)
  )

print(advanced_stats)
print(college_stats)
print(no_degree_stats)

#2.25 c.

cex5_small <- cex5_small %>%
  mutate(ln_foodaway = ifelse(foodaway > 0, log(foodaway), NA))
cex5_small$ln_foodaway

ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram( fill = "blue", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Histogram of ln(Foodaway)",
       x = "ln(Foodaway Spending)",
       y = "Frequency") +
  theme_minimal()

summary_stats <- cex5_small %>%
  summarise(
    Mean_ln_foodaway = mean(ln_foodaway, na.rm = TRUE),
    Median_ln_foodaway = median(ln_foodaway, na.rm = TRUE),
    Q1_ln_foodaway = quantile(ln_foodaway, 0.25, na.rm = TRUE),
    Q3_ln_foodaway = quantile(ln_foodaway, 0.75, na.rm = TRUE)
  )
print(summary_stats)

#2.25 d

model_log <- lm(ln_foodaway ~ income, data = cex5_small)
summary(model_log)
a1 <- coef(model_log)[1]
a2 <- coef(model_log)[2]
a1
a2

#2.25 e

ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  labs(
    title = paste0("ln(foodaway) = ", round(a1, 4), " + ", round(a2, 4), " * income") ,
    x = "household monthly income" , 
    y = "ln(foodaway)"
  )

#2.25 f

cex5_small_filtered <- cex5_small %>% filter(!is.na(ln_foodaway))
cex5_small_filtered$residuals <- residuals(model_log)

ggplot(cex5_small_filtered, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  labs(
    title = "Residuals Plot: ln(FOODAWAY) vs. INCOME",
    x = "Income ($100 units)",
    y = "Residuals"
  ) +
  theme_minimal()
