library(tidyr)
library(dplyr)
library(mvtnorm)
library(rstan)
nobs <- 100

mu <- c(1, 0, -2)

corrmat <- matrix(c(1, 0.1, 0.2, 0.1, 1, -0.5, 0.2, -0.5, 1), 3, 3)

set.seed(101)
tval <- mvtnorm::rmvt(n=nobs, sigma=corrmat, df=7.3)

lval <- log(pt(tval, df=7.3)/(1-pt(tval, df=7.3)))

z <- t(mu + t(lval))

## fake data
dd <- as.matrix(as.data.frame(lapply(as.data.frame(z > 0), as.numeric)))

standata <- list(
	N=nobs,
	K=1,
	D=3,
	y=dd,
	x=matrix(1, nobs, 1),
	nu=7.3
)

rt <- stanc(file="../model/multivariate_logit.stan")
sm <- stan_model(stanc_ret = rt, verbose=FALSE)

fit <- sampling(sm, data=standata, chains=1, iter=2000, seed=101)

save("fit", file="logit_t.rda")

ee <- rstan::extract(fit)

ee$z
