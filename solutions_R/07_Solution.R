library(tidyverse)
library(dagitty)
library(ggplot2)

# Load the data
data <- readRDS("../cdsba-tiramdasg/Causal_Data_Science_Data/membership.rds")

# Part 1
summary(data)
str(data)
cor(data)

# DAG
dag <- dagitty("dag {
  avg_purch -> card
  age -> card
  sex -> card
  pre_avg_purch -> card
}")

plot(dag)


# Part 2
model_naive <- lm(card ~ avg_purch, data = data)
summary(model_naive)

# Part 3

library(MatchIt)

# 1. (Coarsened) Exact Matching

# Matching
cem <- matchit(card ~ avg_purch + age + sex + pre_avg_purch,
               data = data, 
               method = 'cem', 
               estimand = 'ATE')
summary(cem)

matched_data_cem <- match.data(cem)
# Estimation
model_cem <- lm(avg_purch ~ card + age + sex + pre_avg_purch, data = matched_data_cem, weights = weights)
summary(model_cem)

# Custom coarsening
# (1) Matching
cutpoints <- list(age = seq(16, 25, 60), sex = seq(0, 1))
cem_coars <- matchit(card ~ avg_purch + age + sex + pre_avg_purch,
                     data = data, 
                     method = 'cem', 
                     estimand = 'ATE',
                     cutpoints = cutpoints)

# Covariate balance
summary(cem_coars)

# Use matched data
df_cem_coars <- match.data(cem_coars)

# Plot grid
ggplot(df_cem_coars, aes(x = age, y = sex,
                         size = weights, color = as.factor(card))) +
  geom_point(alpha = .2) +
  geom_abline(data.frame(y = cutpoints$sex),
              mapping = aes(intercept = y, slope = 0), 
              linewidth = 1.5, color = "lightblue") +
  geom_vline(data.frame(y = cutpoints$age),
             mapping = aes(xintercept = y),
             linewidth = 1.5, color = "orange") +
  theme(legend.position = "none")
# (2) Estimation
model_cem_coars <- lm(avg_purch ~ card + age + sex + pre_avg_purch, data = matched_data_cem, weights = weights)
summary(model_cem_coars)

# 2. Nearest-Neighbor Matching

# (1) Matching
# replace: one-to-one or one-to-many matching
nn <- matchit(card ~ avg_purch + age + sex + pre_avg_purch,
              data = data,
              method = "nearest",
              distance = "mahalanobis",
              replace = T)

# Covariate Balance
summary(nn)
# Use matched data
df_nn <- match.data(nn)

# (2) Estimation
model_nn <- lm(avg_purch ~ card + age + sex + pre_avg_purch, data = df_nn, weights = weights)
summary(model_nn)

# 3. Inverse Probability Weighting

# (1) Propensity scores
model_prop <- glm(card ~ avg_purch + age + sex + pre_avg_purch,
                  data = data,
                  family = binomial(link = "logit"))
summary(model_prop)

# Add propensities to table
df_aug <- data %>% mutate(propensity = predict(model_prop, type = "response"))
# Extend data by IPW scores
df_ipw <- df_aug %>% mutate(
  ipw = (card/propensity) + ((1-card) / (1-propensity)))

# Look at data with IPW scores
df_ipw %>% 
  select(card, age, sex, avg_purch, pre_avg_purch, propensity, ipw)
# (2) Estimation
model_ipw <- lm(avg_purch ~ card,
                data = df_ipw, 
                weights = ipw)
summary(model_ipw)
