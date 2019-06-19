library(mvtnorm)
library(rstan)

load("../data/logit_t_intercept_slope.rda")

nu <- 7.3

standata <- list(
	N=nrow(simulate_dd),
	K=2,
	D=ncol(simulate_dd),
	y=simulate_dd,
	x=cbind(matrix(1, nrow(simulate_dd), 1), var1),
	nu=nu
)

rt <- stanc(file="../model/multivariate_logit.stan")
sm <- stan_model(stanc_ret = rt, verbose=FALSE)

fit <- sampling(sm, data=standata, chains=1, iter=2000, thin=1, seed=101)

ee <- rstan::extract(fit)

inv.logit <- function(x) exp(x)/(1+exp(x))

## importance sampling weighting step
weights <- rep(NA, 1000)
for (i in 1:1000) {
	print(i)
	hat_sigma2 <- pi^2 * (nu - 2)/(3 * nu)
	location <- apply(ee$beta[i,,], 1, function(bmat) standata$x %*% bmat)
	
	e <- qt(inv.logit(ee$z[i,,] - location), df=nu)
	
	weights[i] <- exp(sum(dmvt(e, sigma=ee$Omega[i,,], df=nu)) - 
		sum(sapply(1:500, function(j) dmvt(ee$z[i,j,], delta=location[j,], sigma=hat_sigma2 * ee$Omega[i,,], df=nu))) +
		sum(sapply(1:500, function(j) dlogis(ee$z[i,j,], location=location[j,], log=TRUE))) -
		sum(dt(e, df=nu, log=TRUE)))
}

save("fit", "weights", file="logit_t_intercept_slope_logit_t.rda")

## stuff
apply(ee$beta, 2:3, wquant, weights=weights, prob=0.5)
apply(ee$beta, 2:3, wquant, weights=weights, prob=0.025)
apply(ee$beta, 2:3, wquant, weights=weights, prob=0.975)
