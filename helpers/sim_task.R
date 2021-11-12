sim_task = function(stimuli, ...){
  kwargs = list(...)
  if (!("nonDecisionTime" %in% kwargs)){
    kwargs$nonDecisionTime = 0
  }
  if (!("barrierDecay" %in% kwargs)){
    kwargs$barrierDecay = 0
  }
  
  # Create placeholder output df
  out = data.frame(EVLeft = NA, EVRight = NA, QVLeft = NA, QVRight = NA, probFractalDraw = NA, choice = NA, reactionTime = NA)
  
  # Loop through all trials of a subjects data (or just all the rows of the input)
  for(i in 1:nrow(sub_data)) {
    
    # Simulate RT and choice for a single trial with given DDM parameters and trial stimulus values
    cur_out = sim_trial(d=kwargs$d, sigma = kwargs$sigma, nonDecisionTime = kwargs$nonDecisionTime, barrierDecay = kwargs$barrierDecay,
                        EVLeft=stimuli$EVLeft[i], EVRight = stimuli$EVRight[i], 
                        QVLeft = stimuli$QVLeft[i], QVRight= stimuli$QVRight[i] , 
                        probFractalDraw = stimuli$probFractalDraw[i])
    
    # Append the trial to the rest of the output
    out = rbind(out, cur_out)
    
  }
  
  # Drops only first row instead of all NAs to keep trials where iterations timed out and a decision was not made
  out = out[-1,]
  
  return(out)
}