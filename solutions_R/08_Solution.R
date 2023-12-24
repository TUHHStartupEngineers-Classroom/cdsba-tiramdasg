# Part 1
library(dplyr)

hosp_data <- readRDS("../cdsba-tiramdasg/Causal_Data_Science_Data/hospdd.rds")
head(hosp_data)
summary(hosp_data)

mean_satis_treated_before <- hosp_data %>%
  filter(procedure == 0, frequency %% 2 != 0) %>%
  pull(satis) %>%
  mean()

mean_satis_treated_after <- hosp_data %>%
  filter(procedure == 0, frequency %% 2 == 0)  %>%
  pull(satis) %>%
  mean()

mean_satis_control_before <- hosp_data %>%
  filter(procedure == 1, frequency %% 2 != 0) %>%
  pull(satis) %>%
  mean()

mean_satis_control_after <- hosp_data %>%
  filter(procedure == 1, frequency %% 2 == 0)  %>%
  pull(satis) %>%
  mean()
cat("Mean Satisfaction for Treated Hospitals Before Treatment:", mean_satis_treated_before, "\n")
cat("Mean Satisfaction for Treated Hospitals After Treatment:", mean_satis_treated_after, "\n")
cat("Mean Satisfaction for Control Hospitals Before Treatment:", mean_satis_control_before, "\n")
cat("Mean Satisfaction for Control Hospitals After Treatment:", mean_satis_control_after, "\n")

# Part 2

model_1 <- lm(satis ~ frequency + month + hospital, data = hosp_data)
summary(model_1)

model_2 <- lm(satis ~ frequency + as.factor(month) + as.factor(hospital), data = hosp_data)
summary(model_2)


