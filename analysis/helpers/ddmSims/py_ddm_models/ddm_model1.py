from addm_toolbox.ddm import DDMTrial
from helpers.ddmSims.py_ddm_models.util import load_trial_conditions_from_csv
import numpy as np
from multiprocessing import Pool
from scipy.stats import norm


class DDM(object):
    """
    Implementation of the traditional drift-diffusion model (DDM), as described
    by Ratcliff et al. (1998).
    """
    def __init__(self, d, sigma, barrier=1, nonDecisionTime=0, bias=0):
        """
        Args:
          d: float, parameter of the model which controls the speed of
              integration of the signal.
          sigma: float, parameter of the model, standard deviation for the
              normal distribution.
          barrier: positive number, magnitude of the signal thresholds.
          nonDecisionTime: non-negative integer, the amount of time in
              milliseconds during which only noise is added to the decision
              variable.
          bias: number, corresponds to the initial value of the decision
              variable. Must be smaller than barrier.
        """
        if barrier <= 0:
            raise ValueError("Error: barrier parameter must larger than zero.")
        if bias >= barrier:
            raise ValueError("Error: bias parameter must be smaller than "
                             "barrier parameter.")
        self.d = d
        self.sigma = sigma
        self.barrier = barrier
        self.nonDecisionTime = nonDecisionTime
        self.bias = bias
        self.params = (d, sigma)


    def get_trial_likelihood(self, trial, timeStep=10, approxStateStep=0.1,
                             plotTrial=False):
        """
        Computes the likelihood of the data from a single DDM trial for these
        particular DDM parameters.
        Args:
          trial: DDMTrial object.
          timeStep: integer, value in milliseconds to be used for binning the
              time axis.
          approxStateStep: float, to be used for binning the RDV axis.
          plotTrial: boolean, flag that determines whether the algorithm
              evolution for the trial should be plotted.
        Returns:
          The likelihood obtained for the given trial and model.
        """
        # Get the number of time steps for this trial.
        numTimeSteps = trial.RT // timeStep
        if numTimeSteps < 1:
            raise RuntimeError(u"Trial response time is smaller than time "
                               "step.")

        # The values of the barriers can change over time.
        decay = 0  # decay = 0 means barriers are constant.
        barrierUp = self.barrier * np.ones(numTimeSteps)
        barrierDown = -self.barrier * np.ones(numTimeSteps)
        for t in range(1, numTimeSteps):
            barrierUp[t] = self.barrier / (1 + (decay * t))
            barrierDown[t] = -self.barrier / (1 + (decay * t))

        # Obtain correct state step.
        halfNumStateBins = np.ceil(self.barrier / approxStateStep)
        stateStep = self.barrier / (halfNumStateBins + 0.5)

        # The vertical axis is divided into states.
        states = np.arange(barrierDown[0] + (stateStep / 2),
                           barrierUp[0] - (stateStep / 2) + stateStep,
                           stateStep)

        # Find the state corresponding to the bias parameter.
        biasState = np.argmin(np.absolute(states - self.bias))

        # Initial probability for all states is zero, except the bias state,
        # for which the initial probability is one.
        prStates = np.zeros((states.size, numTimeSteps))
        prStates[biasState,0] = 1

        # The probability of crossing each barrier over the time of the trial.
        probUpCrossing = np.zeros(numTimeSteps)
        probDownCrossing = np.zeros(numTimeSteps)

        changeMatrix = np.subtract(states.reshape(states.size, 1), states)
        changeUp = np.subtract(barrierUp, states.reshape(states.size, 1))
        changeDown = np.subtract(barrierDown, states.reshape(states.size, 1))

        elapsedNDT = 0

        # Iterate over the time of this trial.
        for time in range(1, numTimeSteps):
            # We use a normal distribution to model changes in RDV
            # stochastically. The mean of the distribution (the change most
            # likely to occur) is calculated from the model parameter d and
            # from the item values, except during non-decision time, in which
            # the mean is zero.
            if elapsedNDT < self.nonDecisionTime // timeStep:
                mean = 0
                elapsedNDT += 1
            else:
                mean = self.d * (trial.valueLeft - trial.valueRight)

            # Update the probability of the states that remain inside the
            # barriers. The probability of being in state B is the sum, over
            # all states A, of the probability of being in A at the previous
            # time step times the probability of changing from A to B. We
            # multiply the probability by the stateStep to ensure that the area
            # under the curves for the probability distributions probUpCrossing
            # and probDownCrossing add up to 1.
            prStatesNew = (stateStep *
                           np.dot(norm.pdf(changeMatrix, mean, self.sigma),
                           prStates[:,time-1]))
            prStatesNew[(states >= barrierUp[time]) |
                        (states <= barrierDown[time])] = 0

            # Calculate the probabilities of crossing the up barrier and the
            # down barrier. This is given by the sum, over all states A, of the
            # probability of being in A at the previous timestep times the
            # probability of crossing the barrier if A is the previous state.
            tempUpCross = np.dot(
                prStates[:,time-1],
                (1 - norm.cdf(changeUp[:, time], mean, self.sigma)))
            tempDownCross = np.dot(
                prStates[:,time-1],
                norm.cdf(changeDown[:, time], mean, self.sigma))

            # Renormalize to cope with numerical approximations.
            sumIn = np.sum(prStates[:,time-1])
            sumCurrent = np.sum(prStatesNew) + tempUpCross + tempDownCross
            prStatesNew = prStatesNew * sumIn / sumCurrent
            tempUpCross = tempUpCross * sumIn / sumCurrent
            tempDownCross = tempDownCross * sumIn / sumCurrent

            # Update the probabilities of each state and the probabilities of
            # crossing each barrier at this timestep.
            prStates[:, time] = prStatesNew
            probUpCrossing[time] = tempUpCross
            probDownCrossing[time] = tempDownCross

        # Compute the likelihood contribution of this trial based on the final
        # choice.
        likelihood = 0
        if trial.choice == -1:  # Choice was left.
            if probUpCrossing[-1] > 0:
                likelihood = probUpCrossing[-1]
        elif trial.choice == 1:  # Choice was right.
            if probDownCrossing[-1] > 0:
                likelihood = probDownCrossing[-1]

        if plotTrial:
            currTime = datetime.now().strftime(u"%Y-%m-%d_%H:%M:%S")
            fileName = u"ddm_trial_" + currTime + u".pdf"
            self.plot_trial(trial.valueLeft, trial.valueRight, timeStep,
                            numTimeSteps, prStates, probUpCrossing,
                            probDownCrossing, fileName=fileName)

        return likelihood
    
    def get_model_log_likelihood(self, trialConditions, numSimulations,
                                 histBins, dataHistLeft, dataHistRight):
        """
        Computes the log-likelihood of a data set given the model. Data set is
        provided in the form of response time histograms conditioned on choice.
        Args:
          trialConditions: list of pairs corresponding to the different trial
              conditions. Each pair contains the values of left and right
              items.
          numSimulations: integer, number of simulations per trial condition to
              be generated when creating response time histograms.
          histBins: list of numbers corresponding to the time bins used to
              create the response time histograms.
          dataHistLeft: dict indexed by trial condition (where each trial
              condition is a pair (valueLeft, valueRight)). Each entry is a
              numpy array corresponding to the response time histogram
              conditioned on left choice for the data. It is assumed that this
              histogram was created using the same time bins as argument
              histBins.
          dataHistRight: same as dataHistLeft, except that the response time
              histograms are conditioned on right choice.
          Returns:
              The log-likelihood for the data given the model.
        """
        logLikelihood = 0
        for trialCondition in trialConditions:
            RTsLeft = list()
            RTsRight = list()
            sim = 0
            while sim < numSimulations:
                try:
                    ddmTrial = self.simulate_trial(trialCondition[0],trialCondition[1],trialCondition[2],trialCondition[3],trialCondition[4])
                except:
                    print(u"An exception occurred while generating "
                          "artificial trial " + str(sim) + u" for condition " +
                          str(trialCondition[0]) + u", " +
                          str(trialCondition[1]) + u", during the " +
                          u"log-likelihood computation for model " +
                          str(self.params) + u".")
                    raise
                if ddmTrial.choice == -1:
                    RTsLeft.append(ddmTrial.RT)
                elif ddmTrial.choice == 1:
                    RTsRight.append(ddmTrial.RT)
                sim += 1

            simulLeft = np.histogram(RTsLeft, bins=histBins)[0]
            if np.sum(simulLeft) != 0:
                simulLeft = simulLeft / np.sum(simulLeft)
            with np.errstate(divide=u"ignore"):
                logSimulLeft = np.where(simulLeft > 0, np.log(simulLeft), 0)
            dataLeft = np.array(dataHistLeft[trialCondition])
            logLikelihood += np.dot(logSimulLeft, dataLeft)

            simulRight = np.histogram(RTsRight, bins=histBins)[0]
            if np.sum(simulRight) != 0:
                simulRight = simulRight / np.sum(simulRight)
            with np.errstate(divide=u"ignore"):
                logSimulRight = np.where(simulRight > 0, np.log(simulRight), 0)
            dataRight = np.array(dataHistRight[trialCondition])
            logLikelihood += np.dot(logSimulRight, dataRight)

        return logLikelihood


    def parallel_get_likelihoods(self, ddmTrials, timeStep=10, stateStep=0.1,
                                 numThreads=4):
        """
        Uses a threadpool to compute the likelihood of the data from a set of
        DDM trials given the DDM parameters.
        Args:
          ddmTrials: list of DDMTrial objects.
          timeStep: integer, value in milliseconds to be used for binning the
              time axis.
          stateStep: float, to be used for binning the RDV axis.
          numThreads: int, number of threads to be used in the threadpool.
        Returns:
          A list of likelihoods obtained for the given trials and model.
        """
        pool = Pool(numThreads)
        likelihoods = pool.map(unwrap_ddm_get_trial_likelihood,
                               zip([self] * len(ddmTrials),
                                   ddmTrials,
                                   [timeStep] * len(ddmTrials),
                                   [stateStep] * len(ddmTrials)))
        pool.close()
        return likelihoods


    def simulate_trial(self, QVLeft, QVRight, EVLeft, EVRight, probFractalDraw, timeStep=10):
        """
        Generates a DDM trial given the item values.
        Args:
          valueLeft: value of the left item.
          valueRight: value of the right item.
          timeStep: integer, value in milliseconds to be used for binning the
              time axis.
        Returns:
          A DDMTrial object resulting from the simulation.
        """
        RDV = self.bias
        time = 0
        elapsedNDT = 0
        
        # Paradigm specific change
        valueLeft = probFractalDraw*QVLeft + (1-probFractalDraw)*(EVLeft)
        valueRight = probFractalDraw*QVRight + (1-probFractalDraw)*(EVRight)
        
        while True:
            # If the RDV hit one of the barriers, the trial is over.
            if RDV >= self.barrier or RDV <= -self.barrier:
                RT = time * timeStep
                if RDV >= self.barrier:
                    choice = -1
                elif RDV <= -self.barrier:
                    choice = 1
                break

            if elapsedNDT < self.nonDecisionTime // timeStep:
                mean = 0
                elapsedNDT += 1
            else:
                mean = self.d * (valueLeft - valueRight)

            # Sample the change in RDV from the distribution.
            RDV += np.random.normal(mean, self.sigma)

            time += 1

        return DDMTrial(RT, choice, valueLeft, valueRight)


    def plot_trial(self, valueLeft, valueRight, timeStep, numTimeSteps,
                   probStates, probUpCrossing, probDownCrossing,
                   fileName=None):
        """
        Plots the likelihood computation for a trial and saves the figures to a
        PDF file.
        Args:
          valueLeft: value of the left item.
          valueRight: value of the right item.
          timeStep: integer, value in milliseconds used in the computation for
              binning the time axis.
          numTimeSteps: integer, number of time steps in the trial.
          probStates: 2-dimensional numpy array with size S x T, where S is the
              number of states and T is the number of time steps. Each value
              corresponds to the probability of the signa being at a specific
              state at a specific time in the trial. 
          probUpCrossing: numpy array with size T, where T is the number of
              time steps. Each value corresponds to the probability of crossing
              the upper barrier over the time of the trial.
          probDownCrossing: numpy array with size T, where T is the number of
              time steps. Each value corresponds to the probability of crossing
              the lower barrier over the time of the trial.
          fileName: string, name of the PDF file to save.
        """
        if not fileName:
            currTime = datetime.now().strftime(u"%Y-%m-%d_%H:%M:%S")
            fileName = u"trial_" + currTime + u".pdf"
        pp = PdfPages(fileName)

        title = u"value left = %d, value right = %d" % (valueLeft, valueRight)

        # Choose a suitable normalization constant.
        maxProb = max(probStates[:,3])

        fig, ax = plt.subplots()
        plt.imshow(probStates[::-1,:],
                   extent=[1, numTimeSteps, -self.barrier, self.barrier],
                   aspect=u"auto", vmin=0, vmax=maxProb)
        ax.set_xticklabels([str(x * timeStep) for x in ax.get_xticks()])
        plt.title(title)
        pp.savefig(fig)
        plt.close(fig)

        fig, ax = plt.subplots()
        plt.plot(list(range(1, numTimeSteps + 1)), probUpCrossing,
                 label=u"up", color=u"red")
        plt.plot(list(range(1, numTimeSteps + 1)), probDownCrossing,
                 label=u"down", color=u"green")
        ax.set_xticklabels([str(x * timeStep) for x in ax.get_xticks()])
        plt.xlabel(u"Time")
        plt.ylabel(u"P(crossing)")
        plt.legend()
        plt.title(title)
        pp.savefig(fig)
        plt.close(fig)

        probInner = np.sum(probStates, 0)
        probUp = np.cumsum(probUpCrossing)
        probDown = np.cumsum(probDownCrossing)
        probTotal = probInner + probUp + probDown
        fig, ax = plt.subplots()
        plt.plot(list(range(1, numTimeSteps + 1)), probUp, color=u"red",
                 label=u"up")
        plt.plot(list(range(1, numTimeSteps + 1)), probDown,
                 color=u"green", label=u"down")
        plt.plot(list(range(1, numTimeSteps + 1)), probInner,
                 color=u"yellow", label=u"in")
        plt.plot(list(range(1, numTimeSteps + 1)), probTotal,
                 color=u"blue", label=u"total")
        plt.axis([1, numTimeSteps, 0, 1.1])
        ax.set_xticklabels([str(x * timeStep) for x in ax.get_xticks()])
        plt.xlabel(u"Time")
        plt.ylabel(u"Cumulative probability")
        plt.legend()
        plt.title(title)
        pp.savefig(fig)
        plt.close(fig)

        fig, ax = plt.subplots()
        plt.plot(list(range(1, numTimeSteps + 1)), probTotal - 1)
        ax.set_xticklabels([str(x * timeStep) for x in ax.get_xticks()])
        plt.xlabel(u"Time")
        plt.ylabel(u"Numerical error")
        plt.title(title)
        pp.savefig(fig)
        plt.close(fig)
        
        pp.close()
        
def recover_pars_pta(d, sigma, rangeD, rangeSigma, trialsFileName=None,
         trialsPerCondition=800, numThreads=9, verbose=False):
    """
    Args:
      d: float, DDM parameter for generating artificial data.
      sigma: float, DDM parameter for generating artificial data.
      rangeD: list of floats, search range for parameter d.
      rangeSigma: list of floats, search range for parameter sigma.
      trialsFileName: string, path of trial conditions file.
      trialsPerCondition: int, number of artificial data trials to be
          generated per trial condition.
      numThreads: int, size of the thread pool.
      verbose: boolean, whether or not to increase output verbosity.
    """
    # Load trial conditions.
    if not trialsFileName:
        trialsFileName = ("helpers/ddmSims/test_data/test_trial_conditions.csv")
    trialConditions = load_trial_conditions_from_csv(trialsFileName)

    # Generate artificial data.
    model = DDM(d, sigma)
    trials = list()
    for (QVLeft, QVRight, EVLeft, EVRight, probFractalDraw) in trialConditions:
        for t in range(trialsPerCondition):
            try:
                trials.append(model.simulate_trial(QVLeft, QVRight, EVLeft, EVRight, probFractalDraw))
            except:
                print(u"An exception occurred while generating artificial "
                      "trial " + str(t) + u" for condition (" +
                      str(QVLeft) + u", " + str(QVRight) + u").")
                raise

    # Get likelihoods for all models and all artificial trials.
    numModels = len(rangeD) * len(rangeSigma)
    likelihoods = dict()
    models = list()
    posteriors = dict()
    for d in rangeD:
        for sigma in rangeSigma:
            model = DDM(d, sigma)
            if verbose:
                print(u"Computing likelihoods for model " + str(model.params) +
                      u"...")
            try:
                likelihoods[model.params] = model.parallel_get_likelihoods(
                    trials, numThreads=numThreads)
            except:
                print(u"An exception occurred during the likelihood "
                      "computations for model " + str(model.params) + u".")
                raise
            models.append(model)
            posteriors[model.params] = 1 / numModels

    # Compute the posteriors.
    for t in range(len(trials)):
        # Get the denominator for normalizing the posteriors.
        denominator = 0
        for model in models:
            denominator += (posteriors[model.params] *
                            likelihoods[model.params][t])
        if denominator == 0:
            continue

        # Calculate the posteriors after this trial.
        for model in models:
            prior = posteriors[model.params]
            posteriors[model.params] = (likelihoods[model.params][t] *
                prior / denominator)

    if verbose:
        for model in models:
            print(u"P" + str(model.params) +  u" = " +
                  str(posteriors[model.params]))
        print(u"Sum: " + str(sum(list(posteriors.values()))))
        
    return trials, models, likelihoods, posteriors


def wrap_ddm_get_model_log_likelihood(args):
    """
    Wrapper for DDM.get_model_log_likelihood(), intended for parallel
    computation using a threadpool.
    Args:
      args: a tuple where the first item is a DDM object, and the remaining
          item are the same arguments required by
          DDM.get_model_log_likelihood().
    Returns:
      The output of DDM.get_model_log_likelihood().
    """
    model = args[0]
    return model.get_model_log_likelihood(*args[1:])

def unwrap_ddm_get_trial_likelihood(arg, **kwarg):
    """
    Wrapper for DDM.get_trial_likelihood(), intended for parallel computation
    using a threadpool. This method should stay outside the class, allowing it
    to be pickled (as required by multiprocessing).
    Args:
      params: same arguments required by DDM.get_trial_likelihood().
    Returns:
      The output of DDM.get_trial_likelihood().
    """
    return DDM.get_trial_likelihood(*arg, **kwarg)

def recover_pars_mla(d, sigma, rangeD, rangeSigma, trialsFileName=None, numTrials=10,
         numSimulations=10, binStep=100, maxRT=8000, numThreads=9,
         verbose=False):
    """
    Args:
      d: float, DDM parameter for generating artificial data.
      sigma: float, DDM parameter for generating artificial data.
      rangeD: list of floats, search range for parameter d.
      rangeSigma: list of floats, search range for parameter sigma.
      trialsFileName: string, path of trial conditions file.
      numTrials: int, number of artificial data trials to be generated per
          trial condition.
      numSimulations: int, number of simulations to be generated per trial
          condition, to be used in the RT histograms.
      binStep: int, size of the bin step to be used in the RT histograms.
      maxRT: int, maximum RT to be used in the RT histograms.
      numThreads: int, size of the thread pool.
      verbose: boolean, whether or not to increase output verbosity.
    """
    pool = Pool(numThreads)

    histBins = list(range(0, maxRT + binStep, binStep))

    # Load trial conditions.
    if not trialsFileName:
        trialsFileName = ("helpers/ddmSims/test_data/test_trial_conditions.csv")
    trialConditions = load_trial_conditions_from_csv(trialsFileName)

    # Generate artificial data.
    dataRTLeft = dict()
    dataRTRight = dict()
    for trialCondition in trialConditions:
        dataRTLeft[trialCondition] = list()
        dataRTRight[trialCondition] = list()
    model = DDM(d, sigma)
    for trialCondition in trialConditions:
        t = 0
        while t < numTrials:
            try:
                trial = model.simulate_trial(
                    trialCondition[0], trialCondition[1], trialCondition[2], trialCondition[3], trialCondition[4])
            except:
                print(u"An exception occurred while generating artificial "
                      "trial " + str(t) + u" for condition " +
                      str(trialCondition[0]) + u", " + str(trialCondition[1]) +
                      u".")
                raise
            if trial.choice == -1:
                dataRTLeft[trialCondition].append(trial.RT)
            elif trial.choice == 1:
                dataRTRight[trialCondition].append(trial.RT)
            t += 1

    # Generate histograms for artificial data.
    dataHistLeft = dict()
    dataHistRight = dict()
    for trialCondition in trialConditions:
        dataHistLeft[trialCondition] = np.histogram(
            dataRTLeft[trialCondition], bins=histBins)[0]
        dataHistRight[trialCondition] = np.histogram(
            dataRTRight[trialCondition], bins=histBins)[0]

    # Grid search on the parameters of the model.
    if verbose:
        print(u"Performing grid search over the model parameters...")
    listParams = list()
    models = list()
    for d in rangeD:
        for sigma in rangeSigma:
            model = DDM(d, sigma)
            models.append(model)
            listParams.append((model, trialConditions, numSimulations,
                              histBins, dataHistLeft, dataHistRight))
    logLikelihoods = pool.map(wrap_ddm_get_model_log_likelihood, listParams)
    pool.close()

    if verbose:
        for i, model in enumerate(models):
            print(u"L" + str(model.params) + u" = " + str(logLikelihoods[i]))
        bestIndex = logLikelihoods.index(max(logLikelihoods))
        print(u"Best fit: " + str(models[bestIndex].params))

    return dataRTLeft, dataRTRight, dataHistLeft, dataHistRight, models, logLikelihoods

