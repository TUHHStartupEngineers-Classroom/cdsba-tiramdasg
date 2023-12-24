library(tidyverse)
# 1

car_data <- readRDS("../cdsba-tiramdasg/Causal_Data_Science_Data/car_prices.rds")
car_data <- na.omit(car_data)
cat("Dimensions of the data:", dim(car_prices), "\n")

# 2

summary(car_data) # may also use str(car_data)
head(car_data)

# 3
car_model <- lm(price ~ doornumber + carbody + enginetype + fuelsystem + stroke + horsepower + citympg, data = car_data) 

# 4

str(car_data$horsepower)
summary(car_model)
summary(car_model)$coefficients['horsepower',]
summary(car_model)$coefficients['horsepower','Pr(>|t|)']

# 5

# Add variable 'seat_heating' and run a regression
car_prices_new <- car_data %>%
  mutate(seat_heating = TRUE) %>%
  na.omit()

car_prices_new$seat_heating <- as.factor(car_prices_new$seat_heating)

# Run a regression with the new variable
model_new <- lm(price ~ doornumber + carbody + enginetype + fuelsystem + stroke + horsepower + citympg + seat_heating, data = car_prices_new)

summary(model_new)
