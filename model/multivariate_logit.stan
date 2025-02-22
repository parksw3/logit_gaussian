functions {
	// two dimensional sum function
	// taken from https://mc-stan.org/docs/2_18/stan-users-guide/multivariate-outcomes.html
  	int sum2d(int[,] a) {
    	int s = 0;
    	for (i in 1:size(a))
      		s += sum(a[i]);
    	return s;
  	}
}

data {
	int<lower=1> K; // number of fixed effects
  	int<lower=1> D; // number of multivariate stuff
  	int<lower=0> N; // number of observations
  	int<lower=0,upper=1> y[N,D]; // observations
  	vector[K] x[N]; // predictors
  	
  	real nu; // degrees of freedom 
}

// define which variables need to be positive and which need to be negative...
transformed data {
  	int<lower=0> N_pos;
  	int<lower=1,upper=N> n_pos[sum2d(y)];
  	int<lower=1,upper=D> d_pos[size(n_pos)];
  	int<lower=0> N_neg;
  	int<lower=1,upper=N> n_neg[(N * D) - size(n_pos)];
  	int<lower=1,upper=D> d_neg[size(n_neg)];

  	N_pos = size(n_pos);
  	N_neg = size(n_neg);
	{
    	int i;
    	int j;
    	i = 1;
    	j = 1;
    	for (n in 1:N) {
      		for (d in 1:D) {
        		if (y[n,d] == 1) {
          			n_pos[i] = n;
          			d_pos[i] = d;
          			i += 1;
        		} else {
          			n_neg[j] = n;
          			d_neg[j] = d;
          			j += 1;
        		}
      		}
		}
  	}
}

parameters {
  	matrix[D, K] beta;
  	cholesky_factor_corr[D] L_Omega; // correlation matrix
  	vector<lower=0>[N_pos] z_pos;
  	vector<upper=0>[N_neg] z_neg;
}

// assigning z parameters
transformed parameters {
  	vector[D] z[N];
  	corr_matrix[D] Omega;
  	for (n in 1:N_pos)
    	z[n_pos[n], d_pos[n]] = z_pos[n];
  	for (n in 1:N_neg)
    	z[n_neg[n], d_neg[n]] = z_neg[n];
    	
    Omega = multiply_lower_tri_self_transpose(L_Omega);
}

model {
	L_Omega ~ lkj_corr_cholesky(4);
 	to_vector(beta) ~ normal(0, 5);
  	{
  		real hat_sigma2 = pi()^2 * (nu - 2)/(3 * nu);
    	vector[D] beta_x[N];
    	for (n in 1:N)
      		beta_x[n] = beta * x[n];
      		
      	z ~ multi_student_t(nu, beta_x, hat_sigma2 * Omega);
  	}
}
