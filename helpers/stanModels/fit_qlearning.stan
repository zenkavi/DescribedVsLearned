
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
  real<lower=0, upper=1> alphas[num_subjs];
  real<lower=0, upper=5> betas[num_subjs];
}


model {
  vector[2] ev; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  
  // priors
  alphas ~ beta(1, 1);
  betas ~ gamma(1, 2);
  
  for(i in 1:num_subjs){
    ev = init_v;
    
    num_trials_for_subj = num_trials[i];

    for (t in 1:num_trials_for_subj) {
      // compute action probabilities
      choices[i, t] ~ bernoulli_logit(betas[i] * (ev[1]-ev[2]));

      // prediction error
      PE[1] = outcomes_left[i, t] - ev[1];
      PE[2] = outcomes_right[i, t] - ev[2];
      
      // value updating (learning)
      ev[1] += alphas[i] * PE[1];
      ev[2] += alphas[i] * PE[2];
    }
  }
}
