library(MASS)
library(rstan)

true_mu <- c(2, 1, -1)
true_corr <- matrix(c(1, 0.7, 0.1, 0.7, 1, -0.4, 0.1, -0.4, 1), 3, 3)
true_sd <- c(1, 1, 1)
true_sigma <- (true_sd %*% t(true_sd)) * true_corr

n <- 500

## checking
cov2cor(true_sigma)

set.seed(101)
dd <- as.data.frame(apply(plogis(mvrnorm(n=n, true_mu, true_sigma)), 2, function(p) rbinom(n=n, 1, p)))

## this seems OK
plogis(true_mu)
sapply(dd, mean)

## not good
cov2cor(cov(dd))

rt <- stanc(file="logit_gaussian.stan")
sm <- stan_model(stanc_ret = rt, verbose=FALSE)

standata <- list(
	N=n,
	V1=dd$V1,
	V2=dd$V2,
	V3=dd$V3,
	sd_1=rep(1, 3),
	prior_only=0
)

fit <- sampling(sm, data=standata, chains=4, iter=2000, thin=10,
				control=list(max_treedepth=15), seed=101)

ee <- extract(fit)

## looks OK
apply(ee$intercept, 2, median)

## reasonable uncertainty
apply(ee$intercept, 2, quantile, 0.025)
apply(ee$intercept, 2, quantile, 0.975)

## large uncertainties in correlation structure
apply(ee$Cor_1, 2, apply, 2, median)
apply(ee$Cor_1, 2, apply, 2, quantile, 0.025)
apply(ee$Cor_1, 2, apply, 2, quantile, 0.975)

## true vs posterior
hist(ee$Cor_1[,2,1])
abline(v=0.7, col=2)

hist(ee$Cor_1[,3,1])
abline(v=0.1, col=2)

hist(ee$Cor_1[,3,2])
abline(v=-0.4, col=2)

## rhat looks OK
hist(bayesplot::rhat(fit))
