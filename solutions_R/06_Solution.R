# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
abtest_data <- readRDS("../cdsba-tiramdasg/Causal_Data_Science_Data/abtest_online.rds")

# Part 1

summary(abtest_data)

ggplot(abtest_data, aes(x = chatbot, y = previous_visit, fill = chatbot)) +
  geom_boxplot() +
  labs(title = "Continuous covariate - previous_visit",
       x = "Chatbot", y = "Previous Visits") +
  theme_minimal()

ggplot(abtest_data, aes(x = mobile_device, fill = chatbot)) +
  geom_bar(position = "dodge") +
  labs(title = "Categorical covariate - mobile_device",
       x = "Mobile Device", y = "Count") +
  theme_minimal()

ggplot(abtest_data, aes(x = as.factor(purchase), fill = chatbot)) +
  geom_bar(position = "dodge") +
  labs(title = "Binary outcome - purchase",
       x = "Purchase", y = "Count") +
  theme_minimal()

# Part 2

model_sales <- lm(purchase_amount ~ chatbot + mobile_device + previous_visit, data = abtest_data)
summary(model_sales)

# Part 3

model_interaction <- lm(purchase_amount ~ chatbot * mobile_device + previous_visit, data = abtest_data)
summary(model_interaction)

# Compute the Conditional Average Treatment Effect (CATE) for mobile users
cate_mobile <- coef(model_interaction)["chatbotTRUE:mobile_deviceTRUE"]
cat("CATE for Mobile Users:", cate_mobile, "\n")

# Part 4

model_logistic <- glm(purchase ~ chatbot + mobile_device + previous_visit, data = abtest_data, family = "binomial")
summary(model_logistic)

# coefficient for chatbot
coef_chatbot <- coef(model_logistic)["chatbotTRUE"]

# Exponentiate the coefficient to get the odds ratio
odds_ratio <- exp(coef_chatbot)

cat("Odds Ratio for Chatbot:", odds_ratio, "\n")
