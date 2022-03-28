data {
  int<lower=1> N;      // Number of subjects
  int<lower=0> Nu_max; // Max (across subjects) number of upper boundary responses
  int<lower=0> Nl_max; // Max (across subjects) number of lower boundary responses
  int<lower=0> Nu[N];  // Number of upper boundary responses for each subj
  int<lower=0> Nl[N];  // Number of lower boundary responses for each subj
  real RTu[N, Nu_max];  // upper boundary response times
  real RTl[N, Nl_max];  // lower boundary response times
  real minRT[N];       // minimum RT for each subject of the observed data
  real RTbound;        // lower bound or RT across all subjects (e.g., 0.1 second)
}

// Model from https://github.com/Seneketh/StanDDM/blob/master/R/StanDDM_NCEN_Pure.r

parameters {
  vector[4] mu_p;
  vector<lower=0>[4] sigma;
  
  // Subject-level raw parameters (for Matt trick)
  vector[N] alpha_pr;
  vector[N] beta_pr;
  vector[N] delta_pr;
  vector[N] tau_pr;
}

transformed parameters {
  // Transform subject-level raw parameters
  vector<lower=0>[N]         alpha; // boundary separation
  vector<lower=0, upper=1>[N] beta;  // initial bias
  vector<lower=0>[N]         delta; // drift rate
  vector<lower=RTbound, upper=max(minRT)>[N] tau; // nondecision time
  
  for (i in 1:N) {
    beta[i] = Phi_approx(mu_p[2] + sigma[2] * beta_pr[i]); //Phi approx so bounded between 0 and 1
    tau[i]  = Phi_approx(mu_p[4] + sigma[4] * tau_pr[i]) * (minRT[i]-RTbound) + RTbound; //non decision time //needs to be necessarily smaller than the RT. The bound makes sure that the sampled min non dec time stays //below the smallest RT by the order of the padding, which gets added after the sampling is done(tis sis why //there is a sum at the end. Phi approx = inverse probit.)
  }
  alpha = exp(mu_p[1] + sigma[1] * alpha_pr); //reparametrization as in Gelman manual second ed pg 313 and Kruschkes manual pg. 281
  delta = exp(mu_p[3] + sigma[3] * delta_pr); //exponential link so only positive values, hard limit on 0
}

model {
  // Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 5);
  
  // Individual parameters for non-centered parameterization
  alpha_pr ~ normal(0, 1);
  beta_pr  ~ normal(0, 1);
  delta_pr ~ normal(0, 1);
  tau_pr   ~ normal(0, 1);
  
  // Begin subject loop
  for (i in 1:N) {
    // Response time distributed along wiener first passage time distribution
    RTu[i, :Nu[i]] ~ wiener(alpha[i], tau[i], beta[i], delta[i]);
    RTl[i, :Nl[i]] ~ wiener(alpha[i], tau[i], 1-beta[i], -delta[i]);
    
  } // end of subject loop
}

generated quantities {
  
  // For log likelihood calculation
  real log_lik[N];
  
  
  { // local section, this saves time and space
  // Begin subject loop
  for (i in 1:N) {
    log_lik[i] = wiener_lpdf(RTu[i, :Nu[i]] | alpha[i], tau[i], beta[i], delta[i]);
    log_lik[i] = log_lik[i] + wiener_lpdf(RTl[i, :Nl[i]] | alpha[i], tau[i], 1-beta[i], -delta[i]);
  }
  }
}
