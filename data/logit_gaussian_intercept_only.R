library(mvtnorm)
nobs <- 500

mu <- c(1, 0, -1)

corrmat <- matrix(c(1, 0.1, 0.3, 0.1, 1, -0.5, 0.3, -0.5, 1), 3, 3)

set.seed(101)
eval <- mvtnorm::rmvnorm(n=nobs, sigma=corrmat)

z <- t(mu + t(eval))

## fake data
simulate_dd <- as.matrix(as.data.frame(lapply(as.data.frame(z > 0), as.numeric)))

save("simulate_dd", file="logit_gaussian_intercept_only.rda")
