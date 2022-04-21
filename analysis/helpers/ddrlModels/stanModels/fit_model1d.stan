
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
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
  // RL group parameter
  real<lower=0.0000000001, upper=1> g_alpha;
  
  // DDM group parameters
  real mu_p;
  real<lower=0> sigma;
  real g_d_pr;
  
  real<lower=0> g_s;
  
  // Prob distortion group parameter
  real<lower=0, upper=1> g_theta;
  
  // Subject parameters
  real<lower=0.0000000001, upper=1> alpha[num_subjs];
  real<lower=0.0000000001, upper=1> d[num_subjs];
  real<lower=0.0000000001, upper=1> s[num_subjs];
  real<lower=0, upper=1> theta[num_subjs];
}

transformed parameters{
  real g_d;
  g_d = exp(mu_p + sigma * g_d_pr);
}

model {
  vector[2] opt_val; 
  vector[2] qv; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  real w_pi;
  real val_diff;
  
  // priors
  g_alpha ~ beta(1, 1);
  
  mu_p  ~ normal(0, 1);
  sigma ~ cauchy(0, 5);
  g_d_pr ~ normal(0, 1);
  
  g_s ~ cauchy(0, 5);
  
  g_theta ~ gamma(1, 5);
  
  alpha ~ normal(g_alpha, 1);
  d ~ normal(g_d, 1);
  s ~ normal(g_s, 1);
  theta ~ normal(g_theta, 1);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    qv = init_v;
    
    for (t in 1:num_trials_for_subj) {
      
      //single parameter linear prob distortion
      w_pi = theta[i]*trial_pFrac[i, t];
      
      // Asymmetric distortion: None of lottery EVs; only distorting QVs
      opt_val[1] = ((1-trial_pFrac[i, t]) * ev_left[i, t]) + (w_pi * qv[1]); 
      opt_val[2] = ((1-trial_pFrac[i, t]) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      val_diff = opt_val[1] - opt_val[2];
      
      // increment target with the following likelihood function:
      if(choices[i, t] == 1){
        response_times[i, t] ~ wiener(2*s[i], 0.1, .5, d[i]*val_diff*s[i]);
      }
      
      if(choices[i, t] == -1){
        response_times[i, t] ~ wiener(2*s[i], 0.1, .5, (-1)*(d[i]*val_diff*s[i]));
      }
      
      PE[1] = fractal_outcomes_left[i, t] - qv[1];
      PE[2] = fractal_outcomes_right[i, t] - qv[2];
      
      // value updating (learning) of both options
      qv[1] += alpha[i] * PE[1];
      qv[2] += alpha[i] * PE[2];
      
    }
  }
}
