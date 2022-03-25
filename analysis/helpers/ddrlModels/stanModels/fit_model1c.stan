
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  // int<lower=1> num_time_steps[num_subjs, 300]; 
  real response_times[num_subjs, 300]; 
  real fractal_outcomes_left[num_subjs, 300];
  real fractal_outcomes_right[num_subjs, 300];
  real ev_left[num_subjs, 300];
  real ev_right[num_subjs, 300];
  real trial_pFrac[num_subjs, 300];
}

transformed data {
  vector[2] init_v;  
  init_v = rep_vector(0.0, 2); // initial values for QV
}

parameters {
  // Declare all parameters as vectors for vectorizing
  real<lower=0.0000000001, upper=1> g_alpha;
  real<lower=0.0000000001, upper=1> g_d;
  // real<lower=0, upper=1> g_sigma;
  real<lower=0, upper=10> g_delta;
  real<lower=0.0000000001, upper=1> alpha[num_subjs];
  real<lower=0.0000000001, upper=1> d[num_subjs];
  // real<lower=0, upper=1> sigma[num_subjs];
  real<lower=0, upper=10> delta[num_subjs];
}


model {
  vector[2] opt_val; 
  vector[2] qv; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  real w_pi;
  real val_diff;
  
  // priors
  // g_alpha ~ beta(1, 1);
  // g_d ~ beta(1, 1);
  g_alpha ~ exponential(2);
  g_d ~ exponential(2);
  // g_sigma ~ beta(1, 1);
  g_delta ~ gamma(1, 5);
  
  alpha ~ normal(g_alpha, 1);
  d ~ normal(g_d, 1);
  // sigma ~ normal(g_sigma, 1);
  delta ~ normal(g_delta, 1);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    qv = init_v;
    
    for (t in 1:num_trials_for_subj) {
      
      w_pi = (delta[i]*(trial_pFrac[i, t])) / ((delta[i]*(trial_pFrac[i, t])) + (1-trial_pFrac[i, t])); //single parameter prob distortion
      
      opt_val[1] = ((1-trial_pFrac[i, t]) * ev_left[i, t]) + (w_pi * qv[1]); // No distortion of lottery EVs; only distorting QVs
      opt_val[2] = ((1-trial_pFrac[i, t]) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      val_diff = opt_val[1] - opt_val[2];
      // print(val_diff);
      
      // increment target with the following likelihood function:
      // choices[i, t] ~ ddm(num_time_steps[i, t], d[i], sigma[i], val_diff);
      if(choices[i, t] == 1){
        response_times [i, t] ~ wiener(1, 0.1, 0, d[i]*val_diff);
      }
      
      if(choices[i, t] == -1){
        response_times [i, t] ~ wiener(1, 0.1, (1-0), (-1)*(d[i]*val_diff));
      }
      
      PE[1] = fractal_outcomes_left[i, t] - qv[1];
      PE[2] = fractal_outcomes_right[i, t] - qv[2];
      
      // value updating (learning) of both options
      qv[1] += alpha[i] * PE[1];
      qv[2] += alpha[i] * PE[2];
      
    }
  }
}
