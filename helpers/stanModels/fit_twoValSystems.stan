
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real ev_left[num_subjs, 300];
  real ev_right[num_subjs, 300];
  real qv_left[num_subjs, 300];
  real qv_right[num_subjs, 300];
}

parameters {
  // Declare all parameters as vectors for vectorizing
  real<lower=0, upper=1> w_pi[num_subjs];
  real<lower=0, upper=5> beta[num_subjs];
}

model {
  vector[2] opt_val; // expected value
  int num_trials_for_subj;
  
  // priors
  w_pi ~ beta(1, 1);
  beta ~ gamma(1, 2);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];

    for (t in 1:num_trials_for_subj) {
      
      opt_val[1] = (w_pi[i] * ev_left[i, t]) + ((1-w_pi[i]) * qv_left[i, t]);
      opt_val[2] = (w_pi[i] * ev_right[i, t]) + ((1-w_pi[i]) * qv_right[i, t]) ;
      
      // increment target with the following likelihood function:
      choices[i, t] ~ bernoulli_logit(beta[i] * (opt_val[1]-opt_val[2]));
      // p(choice left = 1) = exp(a)/(1+exp(a)) = 1/(1+exp(-a))
      // a = beta * (V_left-V_right)
      // The higher V_left - V_right, the higher p(choice left)
      // The larger abs(beta), the more the value difference is amplified
      
    }
  }
}
