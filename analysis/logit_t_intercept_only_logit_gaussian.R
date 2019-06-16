library(mvtnorm)
library(rstan)

load("../data/logit_t_intercept_only.rda")

nu <- 7.3

standata <- list(
	N=nrow(simulate_dd),
	K=1,
	D=ncol(simulate_dd),
	y=simulate_dd,
	x=matrix(1, nrow(simulate_dd), 1),
	nu=nu
)

rt <- stanc(file="../model/multivariate_gaussian.stan")
sm <- stan_model(stanc_ret = rt, verbose=FALSE)

fit <- sampling(sm, data=standata, chains=1, iter=2000, thin=1, seed=101)

save("fit", file="logit_t_intercept_only_logit_gaussian.rda")
