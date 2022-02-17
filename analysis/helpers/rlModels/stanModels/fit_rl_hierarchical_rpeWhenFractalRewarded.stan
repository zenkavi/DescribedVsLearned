
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real fractal_outcomes_left[num_subjs, 300];
  real fractal_outcomes_right[num_subjs, 300];
  real ev_left[num_subjs, 300];
  real ev_right[num_subjs, 300];
  real trial_pFrac[num_subjs, 300];
  real fractal_draw[num_subjs, 300];
}

transformed data {
  vector[2] init_v;  
  init_v = rep_vector(0.0, 2); // initial values for EV
}

parameters {
  // Declare all parameters as vectors for vectorizing
  real<lower=0, upper=1> g_alpha;
  real<lower=0, upper=20> g_gamma;
  real<lower=0, upper=20> g_delta;
  real<lower=0> g_beta;
  real<lower=0, upper=1> alpha[num_subjs];
  real<lower=0, upper=20> gamma[num_subjs];
  real<lower=0, upper=20> delta[num_subjs];
  real<lower=0> beta[num_subjs];
}


model {
  vector[2] opt_val; 
  vector[2] qv; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  real w_pi;
  
  // priors
  g_alpha ~ beta(1, 1);
  g_gamma ~ gamma(1, 5);
  g_delta ~ gamma(1, 5);
  g_beta ~ gamma(1, 2);
  alpha ~ normal(g_alpha, 1);
  gamma ~ normal(g_gamma, 1);
  delta ~ normal(g_delta, 1);
  beta ~ normal(g_beta, 1);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    qv = init_v;
    
    for (t in 1:num_trials_for_subj) {
      
      w_pi = (delta[i]*(trial_pFrac[i, t]^gamma[i])) / ((delta[i]*(trial_pFrac[i, t]^gamma[i])) + (1-trial_pFrac[i, t])^gamma[i]);
      
      opt_val[1] = ((1-w_pi) * ev_left[i, t]) + (w_pi * qv[1]);
      opt_val[2] = ((1-w_pi) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      // increment target with the following likelihood function:
      choices[i, t] ~ bernoulli_logit(beta[i] * (opt_val[1]-opt_val[2]));
      // p(choice left = 1) = exp(a)/(1+exp(a)) = 1/(1+exp(-a))
      // a = beta * (V_left-V_right)
      // The higher V_left - V_right, the higher p(choice left)
      // The larger abs(beta), the more the value difference is amplified
      
      // update value for both options only when the rewarded attribute is the fractal
      if (fractal_draw[i, t] == 1){
        PE[1] = fractal_outcomes_left[i, t] - qv[1];
        PE[2] = fractal_outcomes_right[i, t] - qv[2];
        
        // value updating (learning) of both options
        qv[1] += alpha[i] * PE[1];
        qv[2] += alpha[i] * PE[2]; 
      }
      
    }
  }
}

generated quantities {
  // compute log likelihood for each subject as the sum loglikelihood for each trial
  vector[num_subjs] logLikelihood;
  
  {// local section to save time
  vector[2] opt_val; 
  vector[2] qv; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  real w_pi;
  real logLikelihood_subj_trial;
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    qv = init_v;
    
    logLikelihood[i] = 0;
    
    for (t in 1:num_trials_for_subj) {
      w_pi = (delta[i]*(trial_pFrac[i, t]^gamma[i])) / ((delta[i]*(trial_pFrac[i, t]^gamma[i])) + (1-trial_pFrac[i, t])^gamma[i]);
      
      opt_val[1] = ((1-w_pi) * ev_left[i, t]) + (w_pi * qv[1]);
      opt_val[2] = ((1-w_pi) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      // compute action probabilities
      logLikelihood_subj_trial = bernoulli_logit_lpmf(choices[i,t] | beta[i] * (opt_val[1]-opt_val[2]));
      
      
      if (fractal_draw[i, t] == 1){
        PE[1] = fractal_outcomes_left[i, t] - qv[1];
        PE[2] = fractal_outcomes_right[i, t] - qv[2];
        
        // value updating (learning) of both options
        qv[1] += alpha[i] * PE[1];
        qv[2] += alpha[i] * PE[2];
      }
      
      logLikelihood[i] += logLikelihood_subj_trial;
    }
  }
  }
}
