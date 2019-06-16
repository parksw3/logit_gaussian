library(mvtnorm)
library(rstan)

load("../data/logit_gaussian_intercept_only.rda")

nu <- 7.3

standata <- list(
	N=nrow(simulate_dd),
	K=1,
	D=ncol(simulate_dd),
	y=simulate_dd,
	x=matrix(1, nrow(simulate_dd), 1),
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
	e <- qt(inv.logit(ee$z[i,,] - t(outer(ee$beta[i,,], standata$x)[,,1])), df=nu)
	
	weights[i] <- exp(sum(dmvt(e, sigma=ee$Omega[i,,], df=nu)) - 
		sum(sapply(1:500, function(j) dmvt(ee$z[i,j,], delta=t(outer(ee$beta[i,,], standata$x)[,j,1]), sigma=hat_sigma2 * ee$Omega[i,,], df=nu))) +
		sum(sapply(1:500, function(j) dlogis(ee$z[i,j,], location=t(outer(ee$beta[i,,], standata$x)[,j,1]), log=TRUE))) -
		sum(dt(e, df=nu, log=TRUE)))
}

save("fit", "weights", file="logit_gaussian_intercept_only_logit_t.rda")
