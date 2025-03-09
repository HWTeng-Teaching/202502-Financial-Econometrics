rm(list = ls())  
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
cps5_small

#2.28 a

summary_stats <- cps5_small %>%
  summarise(
    Mean_WAGE = mean(wage, na.rm = TRUE),
    Median_WAGE = median(wage, na.rm = TRUE),
    SD_WAGE = sd(wage, na.rm = TRUE),
    Min_WAGE = min(wage, na.rm = TRUE),
    Max_WAGE = max(wage, na.rm = TRUE),
    Q1_WAGE = quantile(wage, 0.25, na.rm = TRUE),
    Q3_WAGE = quantile(wage, 0.75, na.rm = TRUE),
    
    Mean_EDUC = mean(educ, na.rm = TRUE),
    Median_EDUC = median(educ, na.rm = TRUE),
    SD_EDUC = sd(educ, na.rm = TRUE),
    Min_EDUC = min(educ, na.rm = TRUE),
    Max_EDUC = max(educ, na.rm = TRUE),
    Q1_EDUC = quantile(educ, 0.25, na.rm = TRUE),
    Q3_EDUC = quantile(educ, 0.75, na.rm = TRUE)
  )

print(summary_stats)

ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  scale_x_continuous(limits = c(0, 150), breaks = seq(0, 150, 10))+
  labs(title = "Histogram of WAGE",
       x = "Wage (Earnings per Hour, $)",
       y = "Frequency") +
  theme_minimal()

ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(fill = "red", color = "black", bins = 15, alpha = 0.7) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 2))+
  labs(title = "Histogram of EDUC",
       x = "Years of Education",
       y = "Frequency") +
  theme_minimal()

#2.28 b c

model_wage_educ <- lm(wage ~ educ, data = cps5_small)
summary(model_wage_educ)

cps5_small$residuals <- residuals(model_wage_educ)

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "blue", alpha = 0.5 ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Residuals Plot: WAGE vs. EDUC",
    x = "Years of Education",
    y = "Residuals"
  ) +
  theme_minimal()

#2.28 d

# Estimate separate regressions
model_male <- lm(wage ~ educ, data = cps5_small %>% filter(female == 0))
model_female <- lm(wage ~ educ, data = cps5_small %>% filter(female == 1))
model_black <- lm(wage ~ educ, data = cps5_small %>% filter(black == 1))
model_white <- lm(wage ~ educ, data = cps5_small %>% filter(black == 0))

comparison_results <- bind_rows(
  tidy(model_male) %>% mutate(Group = "Male"),
  tidy(model_female) %>% mutate(Group = "Female"),
  tidy(model_black) %>% mutate(Group = "Black"),
  tidy(model_white) %>% mutate(Group = "White")
) %>% select(Group, term, estimate, std.error, p.value)

# Print results without additional metadata
print(comparison_results, row.names = FALSE)

kable(comparison_results, digits = 3)

#2.28 e

model_quad <- lm(wage ~ I(educ^2), data = cps5_small)
summary(model_quad)

beta_2 <- coef(model_wage_educ)[2]
beta_2
marginal_12 <-  beta_2 * 12
marginal_16 <-  beta_2 * 16

# Print results
print(paste("Marginal Effect at EDUC = 12:", round(marginal_12, 3)))
print(paste("Marginal Effect at EDUC = 16:", round(marginal_16, 3)))
# Extract coefficients
alpha_2 <- coef(model_quad)[2]
alpha_2
# Compute marginal effects
marginal_12 <-  2 * alpha_2 * 12
marginal_16 <-  2 * alpha_2 * 16

# Print results
print(paste("Marginal Effect at EDUC = 12:", round(marginal_12, 3)))
print(paste("Marginal Effect at EDUC = 16:", round(marginal_16, 3)))


#2.28 f

# Create a sequence of EDUC values for smooth quadratic curve
educ_range <- seq(min(cps5_small$educ), max(cps5_small$educ), length.out = 100)

# Compute fitted values for both models
linear_fit <- predict(model_wage_educ, newdata = data.frame(educ = educ_range))
quad_fit <- predict(model_quad, newdata = data.frame(educ = educ_range))

# Create a data frame for plotting fitted values
fitted_data <- data.frame(
  educ = educ_range,
  linear_wage = linear_fit,
  quad_wage = quad_fit
)

# Plot original data points, linear fit, and quadratic fit
ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "gray", alpha = 0.5) +  # Scatter plot of data
  geom_line(data = fitted_data, aes(x = educ, y = linear_wage), color = "blue", linetype = "dashed", size = 1) +  # Linear model
  geom_line(data = fitted_data, aes(x = educ, y = quad_wage), color = "red", size = 1) +  # Quadratic model
  labs(
    title = "WAGE vs. EDUC: Linear vs. Quadratic Model",
    x = "Years of Education",
    y = "Hourly Wage ($)"
  ) +
  theme_minimal() 
   


summary(model_wage_educ)$adj.r.squared
summary(model_quad)$adj.r.squared

anova(model_wage_educ)
anova(model_quad)

