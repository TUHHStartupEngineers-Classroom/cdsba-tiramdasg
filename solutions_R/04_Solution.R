library(ggplot2)
library(dplyr)

set.seed(210)

n <- 500
times <- 100
phis <- seq(0, 1, .02)
comb <- expand.grid(times = seq(times), n = n, phis)
ncomb <- nrow(comb)

res <- matrix(NA, nrow = ncomb, ncol = 6)
colnames(res) <- c('ix', 'n', 'phi', 'cor', 'tstat', 'pval')

simulate_ar <- function(n, phi, sigma = .1) {
  y <- rep(0, n)
  
  for (t in seq(2, n)) {
    y[t] <- phi*y[t-1] + rnorm(1, 0, sigma)
  }
  
  y
}

for (i in seq(ncomb)) {
  ix <- comb[i, 1]
  n <- comb[i, 2]
  phi <- comb[i, 3]
  
  test <- cor.test(simulate_ar(n, phi = phi), simulate_ar(n, phi = phi))
  res[i, ] <- c(ix, n, phi, test$estimate, test$statistic, test$p.value)
}

dat <- data.frame(res) %>% 
  group_by(phi) %>% 
  summarize(
    avg_abs_corr = mean(abs(cor)),
    avg_abs_tstat = mean(abs(tstat)),
    percent_sig = mean(pval < .05)
  )


ggplot(dat, aes(x = phi, y = percent_sig)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Spurious Correlation: phi vs Avg Abs Correlation",
       x = "X - Phi",
       y = "Y - Avg Abs Correlation") +
  theme_minimal()