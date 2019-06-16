library(mvtnorm)
nobs <- 500

mu <- c(1, 0, -1)

corrmat <- matrix(c(1, 0.1, 0.3, 0.1, 1, -0.5, 0.3, -0.5, 1), 3, 3)

set.seed(101)
tval <- mvtnorm::rmvt(n=nobs, sigma=corrmat, df=7.3)

lval <- log(pt(tval, df=7.3)/(1-pt(tval, df=7.3)))

z <- t(mu + t(lval))

## fake data
simulate_dd <- as.matrix(as.data.frame(lapply(as.data.frame(z > 0), as.numeric)))

save("simulate_dd", file="logit_t_intercept_only.rda")
