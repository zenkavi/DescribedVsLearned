functions {
  real custom_lpdf(real aChoice, int aNumTimeSteps, real aD, real aSigma, real aValDiff){
    
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
    // matrix[aNumTimeSteps, 21] changeUp;
    // matrix[aNumTimeSteps, 21] changeDown;
    matrix[21, aNumTimeSteps] changeUp;
    matrix[21, aNumTimeSteps] changeDown;
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
    changeDown = rep_matrix(-1*to_row_vector(barrier), 21) - rep_matrix(to_vector(states), aNumTimeSteps);
    
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
    
    likelihood = 0.00000000000000000000000001;
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
  // int<lower=-1, upper=1> choices[num_subjs, 300]; // choices cast for each trial in columns
  real choices[num_subjs, 300]; // choices cast for each trial in columns
  int num_time_steps[num_subjs, 300]; // converted from response_times using time_step = 10ms
  real opt_val_diffs[num_subjs, 300]; // computed using previously hierarchically fitted rl model including prob distortion
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
      
      target += custom_lpdf(choices[i, t] | num_time_steps[i, t], d[i], s[i], opt_val_diffs[i, t]);
    }
  }
  
}
