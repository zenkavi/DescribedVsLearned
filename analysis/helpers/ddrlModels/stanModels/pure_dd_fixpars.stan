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
  real mu_p;
  real<lower=0> sigma;
  
  // Subject-level raw parameters (for Matt trick)
  vector[N] delta_pr;
}

transformed parameters {
  // Transform subject-level raw parameters
  vector<lower=0>[N]         delta; // drift rate

  delta = exp(mu_p + sigma * delta_pr); //exponential link so only positive values, hard limit on 0
}

model {
  // Hyperparameters
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 5);
  
  // Individual parameters for non-centered parameterization
  delta_pr ~ normal(0, 1);

  // Begin subject loop
  for (i in 1:N) {
    // Response time distributed along wiener first passage time distribution
    RTu[i, :Nu[i]] ~ wiener(1, .1, 0, delta[i]);
    RTl[i, :Nl[i]] ~ wiener(1, .1, 1, -delta[i]);
    
  } // end of subject loop
}

generated quantities {
  
  // For log likelihood calculation
  real log_lik[N];
  
  
  { // local section, this saves time and space
  // Begin subject loop
  for (i in 1:N) {
    log_lik[i] = wiener_lpdf(RTu[i, :Nu[i]] | , .1, 0, delta[i]);
    log_lik[i] = log_lik[i] + wiener_lpdf(RTl[i, :Nl[i]] | 1, .1, 1, -delta[i]);
  }
  }
}
