
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real ev_left[num_subjs, 300];
  real ev_right[num_subjs, 300];
  real qv_left[num_subjs, 300];
  real qv_right[num_subjs, 300];
  real trial_pFrac[num_subjs, 300];
}

parameters {
  // Declare all parameters as vectors for vectorizing
  real<lower=0, upper=20> gamma[num_subjs];
  real<lower=0, upper=20> delta[num_subjs];
  real<lower=0, upper=5> beta[num_subjs];
}


model {
  vector[2] opt_val; 
  int num_trials_for_subj;
  real w_pi;
  
  // priors
  gamma ~ gamma(1, 5);
  delta ~ gamma(1, 5);
  beta ~ gamma(1, 2);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    
    for (t in 1:num_trials_for_subj) {
      
      w_pi = (delta[i]*(trial_pFrac[i, t]^gamma[i])) / ((delta[i]*(trial_pFrac[i, t]^gamma[i])) + (1-trial_pFrac[i, t])^gamma[i]);
      
      opt_val[1] = ((1-w_pi) * ev_left[i, t]) + (w_pi * qv_left[i, t]);
      opt_val[2] = ((1-w_pi) * ev_right[i, t]) + (w_pi * qv_right[i, t]) ;
      
      // increment target with the following likelihood function:
      choices[i, t] ~ bernoulli_logit(beta[i] * (opt_val[1]-opt_val[2]));
      // p(choice left = 1) = exp(a)/(1+exp(a)) = 1/(1+exp(-a))
      // a = beta * (V_left-V_right)
      // The higher V_left - V_right, the higher p(choice left)
      // The larger abs(beta), the more the value difference is amplified
      
    }
  }
}
