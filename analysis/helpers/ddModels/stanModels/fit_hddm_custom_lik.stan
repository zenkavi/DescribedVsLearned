functions{
  
}

data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real num_time_steps[num_subjs, 300]; // converted from response_times using time_step = 10ms
  real opt_val_diff[num_subjs, 300]; // computed using previously hierarchically fitted rl model including prob distortion
}

parameters{
  real<lower=0.0000000001, upper=1> g_d;
  real<lower=0.0000000001, upper=1> g_s;
  real<lower=0.0000000001, upper=1> d[num_subjs];
  real<lower=0.0000000001, upper=1> s[num_subjs];
}

model{
  int num_trials_for_subj;

  g_d ~ cauchy(0, 5);
  g_s ~ cauchy(0, 5);
  d ~ normal(g_d, 1);
  s ~ normal(g_s, 1);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];

    for (t in 1:num_trials_for_subj) {
      
      choices[i, t] ~ ...;
 
      
    }
  }
  
}