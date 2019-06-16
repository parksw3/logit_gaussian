library(mvtnorm)
library(rstan)

load("../data/logit_gaussian_intercept_only.rda")

standata <- list(
	N=nrow(simulate_dd),
	K=1,
	D=ncol(simulate_dd),
	y=simulate_dd,
	x=matrix(1, nrow(simulate_dd), 1)
)

rt <- stanc(file="../model/multivariate_gaussian.stan")
sm <- stan_model(stanc_ret = rt, verbose=FALSE)

fit <- sampling(sm, data=standata, chains=1, iter=2000, thin=1, seed=101)

save("fit", "weights", file="logit_gaussian_intercept_only_logit_gaussian.rda")
