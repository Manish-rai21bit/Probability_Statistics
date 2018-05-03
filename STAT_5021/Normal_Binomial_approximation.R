n <- 20
theta <- 0.3



x <- 0:n
cdf_val_binom <- pbinom(x, size = n, prob = theta)
cdf_val_norm <- pnorm( (x + 0.5 - n*theta)/sqrt(n*theta*(1-theta)), mean = 0, sd = 1)

plot(x, cdf_val_binom, type = 'l', col = 1, lwd = 4)
lines(x, cdf_val_norm, col = 2, lwd = 1)