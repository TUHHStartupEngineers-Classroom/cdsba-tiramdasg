# Load packages necessary
library(dagitty)
library(ggdag)
library(tidyverse)

# Part 1 

dag <- dagitty("dag {
  ParkingSpots -> Sales
  Location -> ParkingSpots
  Location -> Sales
}")

plot(dag)

# Draw DAG
ggdag(dag)

# Part 2

cust_sat <- readRDS("../cdsba-tiramdasg/Causal_Data_Science_Data/customer_sat.rds")
# 1. regress satisfaction on follow_ups
model1 <- lm(satisfaction ~ follow_ups, data = cust_sat)
summary(model1)

# 2. regress satisfaction on follow_ups and account for subscription
model2 <- lm(satisfaction ~ follow_ups + subscription, data = cust_sat)
summary(model2)

# Part 4

# Not conditioning on subscription
customer_sat_not_cond <- ggplot(cust_sat, aes(x = follow_ups, y = satisfaction)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# Conditioning on subscription
customer_sat_cond <- ggplot(cust_sat, aes(x = follow_ups, y = satisfaction, color = subscription)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "right")

# Plot both
customer_sat_not_cond
customer_sat_cond
