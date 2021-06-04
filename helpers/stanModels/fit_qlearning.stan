
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real outcomes_left[num_subjs, 300];
  real outcomes_right[num_subjs, 300];
}

transformed data {
  vector[2] init_v;  
  init_v = rep_vector(0.0, 2); // initial values for EV
}

parameters {
  // Declare all parameters as vectors for vectorizing
  real<lower=0, upper=1> alpha[num_subjs];
  real<lower=0, upper=5> beta[num_subjs];
}


model {
  vector[2] ev; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  
  // priors
  alpha ~ beta(1, 1);
  beta ~ gamma(1, 2);
  
  for(i in 1:num_subjs){
    ev = init_v;
    
    num_trials_for_subj = num_trials[i];
    
    for (t in 1:num_trials_for_subj) {
      // compute action probabilities
      choices[i, t] ~ bernoulli_logit(beta[i] * (ev[1]-ev[2]));
      // p(choice left = 1) = exp(a)/(1+exp(a)) = 1/(1+exp(-a))
      // a = beta * (EV_left-EV_right)
      // The higher EV_left - EV_right, the higher p(choice left)
      // The larger abs(beta), the more the value difference is amplified
      
      // prediction error for both options (instead of only chosen one)
      // outcome for both options presented on each trial
      PE[1] = outcomes_left[i, t] - ev[1];
      PE[2] = outcomes_right[i, t] - ev[2];
      
      // value updating (learning) of both options
      ev[1] += alpha[i] * PE[1];
      ev[2] += alpha[i] * PE[2];
    }
  }
}

generated quantities {
  // compute log likelihood for each subject as the sum loglikelihood for each trial
  vector[num_subjs] logLikelihood;
  
  {// local section to save time
  vector[2] ev; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  real logLikelihood_subj_trial;
  
  for(i in 1:num_subjs){
    ev = init_v;
    
    num_trials_for_subj = num_trials[i];
    
    for (t in 1:num_trials_for_subj) {
      // compute action probabilities
      logLikelihood_subj = bernoulli_logit_lpmf(choices[i,t] | beta[i] * (ev[1]-ev[2]));
      
      PE[1] = outcomes_left[i, t] - ev[1];
      PE[2] = outcomes_right[i, t] - ev[2];
      
      // value updating (learning) of both options
      ev[1] += alpha[i] * PE[1];
      ev[2] += alpha[i] * PE[2];
      
      logLikelihood[i] += sum(logLikelihood_subj);
    }
  }
  }
}
