random_vars <- readRDS("../cdsba-tiramdasg/Causal_Data_Science_Data/random_vars.rds")

# View(random_vars)

# part 1
expected_value_age <- mean(random_vars$age)
expected_value_income <- mean(random_vars$income)

variance_age <- var(random_vars$age)
variance_income <- var(random_vars$income)

std_dev_age <- sd(random_vars$age)
std_dev_income <- sd(random_vars$income)

cat("Expected Values \nAge: ", expected_value_age,"\nIncome: ", expected_value_income)
cat("Variance \nAge: ", variance_age,"\nIncome: ", variance_income)
cat("Standard Deviation \nAge: ", std_dev_age,"\nIncome: ", std_dev_income)

# part 2

covar_age_income <- cov(random_vars$age, random_vars$income)
corr_age_income <- cor(random_vars$age, random_vars$income)

cat("Covariance: ", covar_age_income, "\nCorrelation: ", corr_age_income)

#part 5
library(dplyr)

random_vars <- random_vars %>%
  filter(!is.na(income), !is.na(age))

cond_exp_income_age_18 <- random_vars %>%
  filter(age <= 18) %>%
  summarise(mean_income = mean(income)) %>%
  pull(mean_income)

cond_exp_income_age_18_65 <- random_vars %>%
  filter(age >= 18, age < 65) %>%
  summarise(mean_income = mean(income)) %>%
  pull(mean_income)

cond_exp_income_age_65 <- random_vars %>%
  filter(age >= 65) %>%
  summarise(mean_income = mean(income)) %>%
  pull(mean_income)

cat("18 and below: ", cond_exp_income_age_18, "\n18 and 65: ", cond_exp_income_age_18_65, "\n65 above: ", cond_exp_income_age_65)
