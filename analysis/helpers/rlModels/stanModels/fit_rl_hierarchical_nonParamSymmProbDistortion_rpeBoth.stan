
data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real fractal_outcomes_left[num_subjs, 300];
  real fractal_outcomes_right[num_subjs, 300];
  real ev_left[num_subjs, 300];
  real ev_right[num_subjs, 300];
  real trial_pFrac[num_subjs, 300];
}

transformed data {
  vector[2] init_v;  
  init_v = rep_vector(0.0, 2); // initial values for EV
}

parameters {
  
  real<lower=0, upper=1> g_alpha;
  real<lower=0, upper=3> g_beta;
  real<lower=0, upper=1> g_w0;
  real<lower=0, upper=1> g_w1;
  real<lower=0, upper=1> g_w2;
  real<lower=0, upper=1> g_w3;
  real<lower=0, upper=1> g_w4;
  real<lower=0, upper=1> g_w5;
  real<lower=0, upper=1> g_w6;
  real<lower=0, upper=1> g_w7;
  real<lower=0, upper=1> g_w8;
  real<lower=0, upper=1> g_w9;
  real<lower=0, upper=1> g_w10;
  
  real<lower=0, upper=1> alpha[num_subjs];
  real<lower=0, upper=3> beta[num_subjs];
  real<lower=0, upper=1> w0[num_subjs];
  real<lower=0, upper=1> w1[num_subjs];
  real<lower=0, upper=1> w2[num_subjs];
  real<lower=0, upper=1> w3[num_subjs];
  real<lower=0, upper=1> w4[num_subjs];
  real<lower=0, upper=1> w5[num_subjs];
  real<lower=0, upper=1> w6[num_subjs];
  real<lower=0, upper=1> w7[num_subjs];
  real<lower=0, upper=1> w8[num_subjs];
  real<lower=0, upper=1> w9[num_subjs];
  real<lower=0, upper=1> w10[num_subjs];
}


model {
  vector[2] opt_val; 
  vector[2] qv; // expected value
  vector[2] PE; // prediction error
  int num_trials_for_subj;
  real w_pi;
  
  // priors
  g_alpha ~ beta(1, 1);
  g_beta ~ gamma(1, 2);
  g_w0 ~ beta(1, 1);
  g_w1 ~ beta(1, 1);
  g_w2 ~ beta(1, 1);
  g_w3 ~ beta(1, 1);
  g_w4 ~ beta(1, 1);
  g_w5 ~ beta(1, 1);
  g_w6 ~ beta(1, 1);
  g_w7 ~ beta(1, 1);
  g_w8 ~ beta(1, 1);
  g_w9 ~ beta(1, 1);
  g_w10 ~ beta(1, 1);
  
  alpha ~ normal(g_alpha, 1);
  beta ~ normal(g_beta, 1);
  w0 ~ normal(g_w0, 1);
  w1 ~ normal(g_w1, 1);
  w2 ~ normal(g_w2, 1);
  w3 ~ normal(g_w3, 1);
  w4 ~ normal(g_w4, 1);
  w5 ~ normal(g_w5, 1);
  w6 ~ normal(g_w6, 1);
  w7 ~ normal(g_w7, 1);
  w8 ~ normal(g_w8, 1);
  w9 ~ normal(g_w9, 1);
  w10 ~ normal(g_w10, 1);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    qv = init_v;
    
    for (t in 1:num_trials_for_subj) {
      
      if(trial_pFrac[i, t] == 0){
        w_pi = trial_pFrac[i, t]*w0[i];
      }
      
      if(trial_pFrac[i, t] == 0.1){
        w_pi = trial_pFrac[i, t]*w1[i];
      }
      
      if(trial_pFrac[i, t] == 0.2){
        w_pi = trial_pFrac[i, t]*w2[i];
      }
      
      if(trial_pFrac[i, t] == 0.3){
        w_pi = trial_pFrac[i, t]*w3[i];
      }
      
      if(trial_pFrac[i, t] == 0.4){
        w_pi = trial_pFrac[i, t]*w4[i];
      }
      
      if(trial_pFrac[i, t] == 0.5){
        w_pi = trial_pFrac[i, t]*w5[i];
      }
      
      if(trial_pFrac[i, t] == 0.6){
        w_pi = trial_pFrac[i, t]*w6[i];
      }
      
      if(trial_pFrac[i, t] == 0.7){
        w_pi = trial_pFrac[i, t]*w7[i];
      }
      
      if(trial_pFrac[i, t] == 0.8){
        w_pi = trial_pFrac[i, t]*w8[i];
      }
      
      if(trial_pFrac[i, t] == 0.9){
        w_pi = trial_pFrac[i, t]*w9[i];
      }
      
      if(trial_pFrac[i, t] == 1){
        w_pi = trial_pFrac[i, t]*w10[i];
      }

      opt_val[1] = ((1-w_pi) * ev_left[i, t]) + (w_pi * qv[1]);
      opt_val[2] = ((1-w_pi) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      // increment target with the following likelihood function:
      choices[i, t] ~ bernoulli_logit(beta[i] * (opt_val[1]-opt_val[2]));
      // p(choice left = 1) = exp(a)/(1+exp(a)) = 1/(1+exp(-a))
      // a = beta * (V_left-V_right)
      // The higher V_left - V_right, the higher p(choice left)
      // The larger abs(beta), the more the value difference is amplified
      
      PE[1] = fractal_outcomes_left[i, t] - qv[1];
      PE[2] = fractal_outcomes_right[i, t] - qv[2];
      
      // value updating (learning) of both options
      qv[1] += alpha[i] * PE[1];
      qv[2] += alpha[i] * PE[2];
      
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
      
      if(trial_pFrac[i, t] == 0){
        w_pi = trial_pFrac[i, t]*w0[i];
      }
      
      if(trial_pFrac[i, t] == 0.1){
        w_pi = trial_pFrac[i, t]*w1[i];
      }
      
      if(trial_pFrac[i, t] == 0.2){
        w_pi = trial_pFrac[i, t]*w2[i];
      }
      
      if(trial_pFrac[i, t] == 0.3){
        w_pi = trial_pFrac[i, t]*w3[i];
      }
      
      if(trial_pFrac[i, t] == 0.4){
        w_pi = trial_pFrac[i, t]*w4[i];
      }
      
      if(trial_pFrac[i, t] == 0.5){
        w_pi = trial_pFrac[i, t]*w5[i];
      }
      
      if(trial_pFrac[i, t] == 0.6){
        w_pi = trial_pFrac[i, t]*w6[i];
      }
      
      if(trial_pFrac[i, t] == 0.7){
        w_pi = trial_pFrac[i, t]*w7[i];
      }
      
      if(trial_pFrac[i, t] == 0.8){
        w_pi = trial_pFrac[i, t]*w8[i];
      }
      
      if(trial_pFrac[i, t] == 0.9){
        w_pi = trial_pFrac[i, t]*w9[i];
      }
      
      if(trial_pFrac[i, t] == 1){
        w_pi = trial_pFrac[i, t]*w10[i];
      }

      opt_val[1] = ((1-w_pi) * ev_left[i, t]) + (w_pi * qv[1]);
      opt_val[2] = ((1-w_pi) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      // compute action probabilities
      logLikelihood_subj_trial = bernoulli_logit_lpmf(choices[i,t] | beta[i] * (opt_val[1]-opt_val[2]));
      
      PE[1] = fractal_outcomes_left[i, t] - qv[1];
      PE[2] = fractal_outcomes_right[i, t] - qv[2];
      
      // value updating (learning) of both options
      qv[1] += alpha[i] * PE[1];
      qv[2] += alpha[i] * PE[2];
      
      logLikelihood[i] += logLikelihood_subj_trial;
    }
  }
  }
}
