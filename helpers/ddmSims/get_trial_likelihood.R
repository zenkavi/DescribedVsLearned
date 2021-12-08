
# This needs to run for all trials of a subject
# Can be run parallel for all trials of a subject
# The within function loop must remain sequential - OR is there a way to speed it up using lin alg?
get_trial_likelihood = function(pars, trial, timeStep=10, approxStateStep=0.1){
  
}

# # Get the number of time steps for this trial.
# numTimeSteps = trial.RT // timeStep
# if numTimeSteps < 1:
#   raise RuntimeError(u"Trial response time is smaller than time "
#                      "step.")
# 
# # The values of the barriers can change over time.
# decay = 0  # decay = 0 means barriers are constant.
# barrierUp = self.barrier * np.ones(numTimeSteps)
# barrierDown = -self.barrier * np.ones(numTimeSteps)
# for t in range(1, numTimeSteps):
#   barrierUp[t] = self.barrier / (1 + (decay * t))
# barrierDown[t] = -self.barrier / (1 + (decay * t))
# 
# # Obtain correct state step.
# halfNumStateBins = np.ceil(self.barrier / approxStateStep)
# stateStep = self.barrier / (halfNumStateBins + 0.5)
# 
# # The vertical axis is divided into states.
# states = np.arange(barrierDown[0] + (stateStep / 2),
#                    barrierUp[0] - (stateStep / 2) + stateStep,
#                    stateStep)
# 
# # Find the state corresponding to the bias parameter.
# biasState = np.argmin(np.absolute(states - self.bias))
# 
# # Initial probability for all states is zero, except the bias state,
# # for which the initial probability is one.
# prStates = np.zeros((states.size, numTimeSteps))
# prStates[biasState,0] = 1
# 
# # The probability of crossing each barrier over the time of the trial.
# probUpCrossing = np.zeros(numTimeSteps)
# probDownCrossing = np.zeros(numTimeSteps)
# 
# changeMatrix = np.subtract(states.reshape(states.size, 1), states)
# changeUp = np.subtract(barrierUp, states.reshape(states.size, 1))
# changeDown = np.subtract(barrierDown, states.reshape(states.size, 1))
# 
# elapsedNDT = 0
# 
# # Iterate over the time of this trial.
# for time in range(1, numTimeSteps):
#   # We use a normal distribution to model changes in RDV
#   # stochastically. The mean of the distribution (the change most
#   # likely to occur) is calculated from the model parameter d and
#   # from the item values, except during non-decision time, in which
#   # the mean is zero.
#   if elapsedNDT < self.nonDecisionTime // timeStep:
#   mean = 0
# elapsedNDT += 1
# else:
#   mean = self.d * (trial.valueLeft - trial.valueRight)
# 
# # Update the probability of the states that remain inside the
# # barriers. The probability of being in state B is the sum, over
# # all states A, of the probability of being in A at the previous
# # time step times the probability of changing from A to B. We
# # multiply the probability by the stateStep to ensure that the area
# # under the curves for the probability distributions probUpCrossing
# # and probDownCrossing add up to 1.
# prStatesNew = (stateStep *
#                  np.dot(norm.pdf(changeMatrix, mean, self.sigma),
#                         prStates[:,time-1]))
# prStatesNew[(states >= barrierUp[time]) |
#               (states <= barrierDown[time])] = 0
# 
# # Calculate the probabilities of crossing the up barrier and the
# # down barrier. This is given by the sum, over all states A, of the
# # probability of being in A at the previous timestep times the
# # probability of crossing the barrier if A is the previous state.
# tempUpCross = np.dot(
#   prStates[:,time-1],
#   (1 - norm.cdf(changeUp[:, time], mean, self.sigma)))
# tempDownCross = np.dot(
#   prStates[:,time-1],
#   norm.cdf(changeDown[:, time], mean, self.sigma))
# 
# # Renormalize to cope with numerical approximations.
# sumIn = np.sum(prStates[:,time-1])
# sumCurrent = np.sum(prStatesNew) + tempUpCross + tempDownCross
# prStatesNew = prStatesNew * sumIn / sumCurrent
# tempUpCross = tempUpCross * sumIn / sumCurrent
# tempDownCross = tempDownCross * sumIn / sumCurrent
# 
# # Update the probabilities of each state and the probabilities of
# # crossing each barrier at this timestep.
# prStates[:, time] = prStatesNew
# probUpCrossing[time] = tempUpCross
# probDownCrossing[time] = tempDownCross
# 
# # Compute the likelihood contribution of this trial based on the final
# # choice.
# likelihood = 0
# if trial.choice == -1:  # Choice was left.
#   if probUpCrossing[-1] > 0:
#   likelihood = probUpCrossing[-1]
# elif trial.choice == 1:  # Choice was right.
#   if probDownCrossing[-1] > 0:
#   likelihood = probDownCrossing[-1]
# 
# return likelihood