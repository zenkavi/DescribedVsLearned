functions {
  real ddm_lpdf(real aChoice, int aNumTimeSteps, real aD, real aSigma, real aValDiff){
    
    vector[aNumTimeSteps] barrier;
    real stateStep = 0.0952381;
    // to do: fix the hard coded states and everything that depends on the size of the states vector
    row_vector[21] states = [-0.9523810, -0.8571429, -0.7619048, -0.6666667, -0.5714286, -0.4761905, -0.3809524, -0.2857143, -0.1904762, -0.0952381, 0.0000000, 0.0952381, 0.1904762, 0.2857143, 0.3809524, 0.4761905, 0.571428, 0.6666667, 0.7619048, 0.8571429, 0.9523810];
    int biasState = 11;
    matrix[21, aNumTimeSteps] prStates;
    vector[aNumTimeSteps] probUpCrossing;
    vector[aNumTimeSteps] probDownCrossing;
    matrix[21, 21] changeMatrix;
    matrix[21, 21] changeMatrixProb;
    matrix[aNumTimeSteps, 21] changeUp;
    matrix[aNumTimeSteps, 21] changeDown;
    vector[21] changeUpProb;
    vector[21] changeDownProb;
    real mu;
    int curTime;
    vector[21] prStatesNew;
    real tempUpCross;
    real tempDownCross;
    real sumIn;
    real sumCurrent;
    real likelihood;

    barrier = rep_vector(1, aNumTimeSteps);
    prStates = rep_matrix(0, 21, aNumTimeSteps);
    prStates[biasState,1] = 1;
    
    prStatesNew = rep_vector(0, 21);
    changeUpProb = rep_vector(0, 21);
    changeDownProb = rep_vector(0, 21);
    
    probUpCrossing = rep_vector(0, aNumTimeSteps);
    probDownCrossing = rep_vector(0, aNumTimeSteps);
    changeMatrix = rep_matrix(to_vector(states), 21) - rep_matrix(states, 21);
    
    changeUp = rep_matrix(to_row_vector(barrier), 21) - rep_matrix(to_vector(states), aNumTimeSteps);
    changeDown = rep_matrix(1*to_row_vector(barrier), 21) - rep_matrix(to_vector(states), aNumTimeSteps);
    
    mu = aD * aValDiff;

    // LOOP of state probability updating up to reaction time
    for(nextTime in 2:aNumTimeSteps){
      curTime = nextTime - 1; 
      
      for(i in 1:21){
        for(j in 1:21){
          changeMatrixProb[i, j] =  exp(normal_lpdf(changeMatrix[i, j] | mu, aSigma));
        }
      }
      
      for(i in 1:21){
        prStatesNew[i] = dot_product(changeMatrixProb[i], prStates[:,curTime]) ;
      }
      prStatesNew = stateStep * prStatesNew;
      
      for(i in 1:21){
        changeUpProb[i] = 1 - normal_cdf(changeUp[i, nextTime], mu, aSigma);
        changeDownProb[i] = normal_cdf(changeDown[i, nextTime], mu, aSigma);
      }
      
      tempUpCross = dot_product(to_row_vector(prStates[:,curTime]), changeUpProb);
      tempDownCross = dot_product(to_row_vector(prStates[:,curTime]), changeDownProb);
      
      
      // Renormalize to cope with numerical approximations.
      sumIn = sum(prStates[:,curTime]);
      sumCurrent = sum(prStatesNew[:]) + tempUpCross + tempDownCross;
      prStatesNew = prStatesNew * sumIn / sumCurrent;
      tempUpCross = tempUpCross * sumIn / sumCurrent;
      tempDownCross = tempDownCross * sumIn / sumCurrent;
      
      // Avoid NAs for likelihood conditional statements
      if (is_nan(tempUpCross)){
        tempUpCross = 0;
      }
      if (is_nan(tempDownCross)){
        tempDownCross = 0;
      }
      
      // Update the probabilities 
      prStates[:, nextTime] = prStatesNew;
      probUpCrossing[nextTime] = tempUpCross;
      probDownCrossing[nextTime] = tempDownCross;
    }
    
    likelihood = 0;
    if (aChoice == 1){ // Choice was left.
    if (probUpCrossing[aNumTimeSteps] > 0){
      likelihood = probUpCrossing[aNumTimeSteps];
    }
    } else if (aChoice == -1){
      if(probDownCrossing[aNumTimeSteps] > 0){
        likelihood = probDownCrossing[aNumTimeSteps];
      } 
    }
    
    return (log(likelihood));
  }
}

data {
  int<lower=1> num_subjs; // number of trials per subject
  int<lower=1> num_trials[num_subjs]; // number of trials per subject
  int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  int<lower=1> num_time_steps[num_subjs, 300]; // choices cast for each trial in columns
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
  real<lower=0, upper=1> g_alpha;
  real<lower=0, upper=1> g_d;
  real<lower=0, upper=1> g_sigma;
  real<lower=0, upper=10> g_delta;
  real<lower=0, upper=1> alpha[num_subjs];
  real<lower=0, upper=1> d[num_subjs];
  real<lower=0, upper=1> sigma[num_subjs];
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
  g_alpha ~ beta(1, 1);
  g_d ~ beta(1, 1);
  g_sigma ~ beta(1, 1);
  g_delta ~ gamma(1, 5);
  alpha ~ normal(g_alpha, 1);
  d ~ normal(g_d, 1);
  sigma ~ normal(g_sigma, 1);
  delta ~ normal(g_delta, 1);
  
  for(i in 1:num_subjs){
    num_trials_for_subj = num_trials[i];
    qv = init_v;
    
    for (t in 1:num_trials_for_subj) {
      
      w_pi = (delta[i]*(trial_pFrac[i, t])) / ((delta[i]*(trial_pFrac[i, t])) + (1-trial_pFrac[i, t])); //single parameter prob distortion
      
      opt_val[1] = ((1-trial_pFrac[i, t]) * ev_left[i, t]) + (w_pi * qv[1]); // No distortion of lottery EVs; only distorting QVs
      opt_val[2] = ((1-trial_pFrac[i, t]) * ev_right[i, t]) + (w_pi * qv[2]) ;
      
      val_diff = opt_val[1] - opt_val[2];
      
      // increment target with the following likelihood function:
      choices[i, t] ~ ddm(num_time_steps[i, t], d[i], sigma[i], val_diff);
      
      PE[1] = fractal_outcomes_left[i, t] - qv[1];
      PE[2] = fractal_outcomes_right[i, t] - qv[2];
      
      // value updating (learning) of both options
      qv[1] += alpha[i] * PE[1];
      qv[2] += alpha[i] * PE[2];
      
    }
  }
}

// generated quantities {
//   // compute log likelihood for each subject as the sum loglikelihood for each trial
//   vector[num_subjs] logLikelihood;
//   
//   {// local section to save time
//   vector[2] opt_val; 
//   vector[2] qv; // expected value
//   vector[2] PE; // prediction error
//   int num_trials_for_subj;
//   real w_pi;
//   real logLikelihood_subj_trial;
//   
//   for(i in 1:num_subjs){
//     num_trials_for_subj = num_trials[i];
//     qv = init_v;
//     
//     logLikelihood[i] = 0;
//     
//     for (t in 1:num_trials_for_subj) {
//       w_pi = (delta[i]*(trial_pFrac[i, t]^gamma[i])) / ((delta[i]*(trial_pFrac[i, t]^gamma[i])) + (1-trial_pFrac[i, t])^gamma[i]);
//       
//       opt_val[1] = ((1-w_pi) * ev_left[i, t]) + (w_pi * qv[1]);
//       opt_val[2] = ((1-w_pi) * ev_right[i, t]) + (w_pi * qv[2]) ;
//       
//       // compute action probabilities
//       // logLikelihood_subj_trial = bernoulli_logit_lpmf(choices[i,t] | beta[i] * (opt_val[1]-opt_val[2]));
//       
//       PE[1] = fractal_outcomes_left[i, t] - qv[1];
//       PE[2] = fractal_outcomes_right[i, t] - qv[2];
//       
//       // value updating (learning) of both options
//       qv[1] += alpha[i] * PE[1];
//       qv[2] += alpha[i] * PE[2];
//       
//       logLikelihood[i] += logLikelihood_subj_trial;
//     }
//   }
//   }
// }