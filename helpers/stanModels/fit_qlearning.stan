
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials; // number of trials per subject
  int<lower=-1, upper=2> choices[num_trials*num_subjs]; // choices cast for each trial in columns
  real outcomes[num_trials*num_subjs, 2];  // for all subjects for both fractals
}

transformed data {
  vector[2] init_v;  
  init_v = rep_vector(0.0, 2); // initial values for EV
}

parameters {
  // Declare all parameters as vectors for vectorizing
  vector[num_subjs] alphas;
  vector[num_subjs] betas;
}


model {
  vector[2] ev; // expected value
  vector[2] PE; // prediction error
  
  // priors
  alphas ~ beta(1, 1);
  betas ~ gamma(1, 2);
  
  for(i in 1:num_subjs){
    ev = init_v;
    
    for (t in 1:num_trials) {
      // compute action probabilities
      choices[t] ~ bernoulli_logit(betas[i] * (ev[2]-ev[1]));

      // prediction error
      PE[1] = outcomes[t, 1] - ev[1];
      PE[2] = outcomes[t, 2] - ev[2];
      
      // value updating (learning)
      ev[1] += alphas[i] * PE[1];
      ev[2] += alphas[i] * PE[2];
    }
  }
}
