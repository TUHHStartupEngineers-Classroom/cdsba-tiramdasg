library(dplyr)

df <- readRDS("../Causal_Data_Science_Data/coupon.rds")
c0 <- 60

# Part1
bw_half <- c0 + c(-2, 2)

# Interval [58,62]
df_bw_below <- df %>% filter(days_since_last >= bw_half[1] & days_since_last < c0)
df_bw_above <- df %>% filter(days_since_last >= c0 & days_since_last <= bw_half[2])
df_bw_half <- bind_rows(df_bw_above, df_bw_below)
# Dimension of data in [58, 62] interval
dim(df_bw_half)

model_bw_below <- lm(purchase_after ~ days_since_last, df_bw_below)
model_bw_above <- lm(purchase_after ~ days_since_last, df_bw_above)

y0 <- predict(model_bw_below, tibble(days_since_last = c0))
y1 <- predict(model_bw_above, tibble(days_since_last = c0))

late <- y1 - y0
sprintf("LATE for half the bandwidth [58,62]: %.2f", late)

# Part 2
bw_double <- c0 + c(-10, 10)

# Interval [50,70]
df_bw_below <- df %>% filter(days_since_last >= bw_double[1] & days_since_last < c0)
df_bw_above <- df %>% filter(days_since_last >= c0 & days_since_last <= bw_double[2])
df_bw_double <- bind_rows(df_bw_above, df_bw_below)
# Dimension of data in [50, 70] interval
dim(df_bw_double)

model_bw_below <- lm(purchase_after ~ days_since_last, df_bw_below)
model_bw_above <- lm(purchase_after ~ days_since_last, df_bw_above)

y0 <- predict(model_bw_below, tibble(days_since_last = c0))
y1 <- predict(model_bw_above, tibble(days_since_last = c0))

late <- y1 - y0
sprintf("LATE for double the bandwidth [50, 70]: %.2f", late)

# Part 3
df <- readRDS("../Causal_Data_Science_Data/shipping.rds")
c0 = 30
library(rddensity)
rddd <- rddensity(df$purchase_amount, c = c0)
rdd_plot <- rdplotdensity(rddd, df$purchase_amount, plotN = 100)