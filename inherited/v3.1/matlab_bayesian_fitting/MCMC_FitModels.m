close all
clear
clc

doparallel = 0;
model1Name = 'model1.txt';
model2Name = 'model2.txt';

parcodes = [1 3:20 22:25 27];

numSessions = 5;

lotteryValue = [];
lotteryProb = [];

probFractalLeft = [];
probFractalRight = [];

probFractalDraw = [];
fractalDraw = [];

responded = [];
choiceLeft = [];
reward = [];
rewardFractalLeft = [];
rewardFractalRight = [];

rt = [];
subj = [];
session = [];
subjTrial = [];

totalNumTrials = 0;

trialCount = 1;
for p=parcodes
    for s=1:numSessions
        if p < 10
            f = sprintf('expdata/expdata_AR-GT-BUNDLES-0%d_s%d_*.mat', ...
                p, s);
        else
            f = sprintf('expdata/expdata_AR-GT-BUNDLES-%d_s%d_*.mat', ...
                p, s);
        end
        files = dir(f);
        load(sprintf('expdata/%s', files(1).name));

        for trial = 1:data.numTrials
            if data.responded(trial) == 1
                lotteryValue(trialCount) = data.lotteryValue(trial);
                lotteryProb(trialCount) = data.lotteryProb(trial);

                probFractalLeft(trialCount) = data.fractalLeftProb(trial);
                probFractalRight(trialCount) = ...
                    data.fractalRightProb(trial);

                probFractalDraw(trialCount) = data.probFractalDraw(trial);
                fractalDraw(trialCount) = data.fractalDraw(trial);

                responded(trialCount) = data.responded(trial);
                choiceLeft(trialCount) = data.choiceLeft(trial);
                reward(trialCount) = data.reward(trial);
                rewardFractalLeft(trialCount) = ...
                    data.leftFractalReward(trial);
                rewardFractalRight(trialCount) = ...
                    data.rightFractalReward(trial);

                rt(trialCount) = data.reactionTime(trial);
                subj(trialCount) = p;
                session(trialCount) = s;
                subjTrial(trialCount) = trial;

                trialCount = trialCount + 1;
                totalNumTrials = totalNumTrials + 1;
            end
        end
    end
end

% Define MCMC parameters for JAGS.
numChains  = 5; % how many chains?
numBurnin  = 10000; % how many burn-in samples?
numSamples = 4000; % how many recorded samples?
numThin = 10; % thinning rate, how often do we keep a sample?

% Set initial values of each parameter in each chain for model 1.
for i = 1:numChains
    S1.alpha = normrnd(0, 10);
    S1.beta1 = normrnd(0, 10);
    S1.beta2 = normrnd(0, 10);
    S1.beta3 = normrnd(0, 10);
    S1.beta4 = normrnd(0, 10);
    S1.lambda = rand;
    initVar1(i) = S1;
end

% Set initial values of each parameter in each chain for model 2.
for i = 1:numChains
    S2.alpha = normrnd(0, 10);
    S2.beta1 = normrnd(0, 10);
    S2.beta2 = normrnd(0, 10);
    S2.lambda = rand;
    initVar2(i) = S2;
end


%% Loop over subjects and estimate model parameters.
for s=parcodes
    % Data to pass to JAGS models.
    datastruct = struct( ...
        'N', length(choiceLeft(subj==s & responded==1)), ...
        'choiceleft', choiceLeft(subj==s & responded==1), ...
        'choice', choiceLeft(subj==s & responded==1), ...
        'rewardleft', rewardFractalLeft(subj==s & responded==1), ...
        'rewardright', rewardFractalRight(subj==s & responded==1), ...
        'probfractaldraw', probFractalDraw(subj==s & responded==1), ...
        'valuelottery', lotteryValue(subj==s & responded==1), ...
        'problottery', lotteryProb(subj==s & responded==1));

    % Run MCMC for model 1.
    [samples1, stats1] = matjags( ...
        datastruct, ... % observed data
        fullfile(pwd, model1Name), ... % file with model definition
        initVar1, ... % initial values for latent variables
        'doparallel' , doparallel, ... % parallelization flag
        'nchains', numChains,... % number of MCMC chains
        'nburnin', numBurnin,... % number of burn-in steps
        'nsamples', numSamples, ... % number of samples to extract
        'thin', numThin, ... % thinning rate
        'monitorparams', {'alpha', 'beta1', 'beta2', 'beta3', ...
            'beta4', 'lambda'}, ... % latent variables
        'savejagsoutput' , 0 , ... % save command line output produced
        'verbosity' , 1 , ...
        'cleanup' , 1 );

    fprintf('\n\n');
    save(sprintf('mcmc_chains/subj%d_model1.mat', s), ...
        'samples1', 'stats1');
    
    % Run MCMC for model 2.
    [samples2, stats2] = matjags( ...
        datastruct, ... % observed data
        fullfile(pwd, model2Name), ... % file with model definition
        initVar2, ... % initial values for latent variables
        'doparallel' , doparallel, ... % parallelization flag
        'nchains', numChains,... % number of MCMC chains
        'nburnin', numBurnin,... % number of burn-in steps
        'nsamples', numSamples, ... % number of samples to extract
        'thin', numThin, ... % thinning rate
        'monitorparams', {'alpha', 'beta1', 'beta2', ...
            'lambda'}, ... % latent variables
        'savejagsoutput' , 0 , ... % save command line output produced
        'verbosity' , 1 , ...
        'cleanup' , 1 );

    fprintf('\n\n');
    save(sprintf('mcmc_chains/subj%d_model2.mat', s), ...
        'samples2', 'stats2');
end


%% Generate posterior distributions for parameter models.

plotsFileBaseName = 'bayesian';
clocknum = clock;
dateString = [];
for i=1:5
    dateString = [dateString '_' num2str(clocknum(i))];
end
plotsFileName = [plotsFileBaseName dateString];

data_path = '/Users/zeynepenkavi/Dropbox/RangelLab/DescribedVsLearned/v3.1/matlab_bayesian_fitting';
for s = parcodes
    load(sprintf('%s/mcmc_chains/subj%d_model1.mat', data_path,s));
    allAlpha_1 = reshape(samples1.alpha, [numel(samples1.alpha),1]);
    allBeta1_1 = reshape(samples1.beta1, [numel(samples1.beta1),1]);
    allBeta2_1 = reshape(samples1.beta2, [numel(samples1.beta2),1]);
    allBeta3_1 = reshape(samples1.beta3, [numel(samples1.beta3),1]);
    allBeta4_1 = reshape(samples1.beta4, [numel(samples1.beta4),1]);
    allLambda_1 = reshape(samples1.lambda, [numel(samples1.lambda),1]);

    figure;
    subplot(2,6,1);
    histogram(allAlpha_1, 'Normalization', 'probability');
    xlabel('alpha','FontSize', 10);
    ylabel('P(alpha)','FontSize', 10);
    set(gca,'FontSize',8);
    
    subplot(2,6,2);
    histogram(allBeta1_1, 'Normalization', 'probability');
    xlabel('beta1','FontSize', 10);
    ylabel('P(beta1)','FontSize', 10);
    set(gca,'FontSize',8);

    subplot(2,6,3);
    histogram(allBeta2_1, 'Normalization', 'probability');
    xlabel('beta2','FontSize', 10);
    ylabel('P(beta2)','FontSize', 10);
    set(gca,'FontSize',8);

    subplot(2,6,4);
    histogram(allBeta3_1, 'Normalization', 'probability');
    xlabel('beta3','FontSize', 10);
    ylabel('P(beta3)','FontSize', 10);
    set(gca,'FontSize',8);

    subplot(2,6,5);
    histogram(allBeta4_1, 'Normalization', 'probability');
    xlabel('beta4','FontSize', 10);
    ylabel('P(beta4)','FontSize', 10);
    set(gca,'FontSize',8);

    subplot(2,6,6);
    histogram(allLambda_1, 'Normalization', 'probability');
    xlabel('lambda','FontSize', 10);
    ylabel('P(lambda)','FontSize', 10);
    set(gca,'FontSize',8);
    
    load(sprintf('%s/mcmc_chains/subj%d_model2.mat', data_path,s));
    allAlpha_2 = reshape(samples2.alpha, [numel(samples2.alpha),1]);
    allBeta1_2 = reshape(samples2.beta1, [numel(samples2.beta1),1]);
    allBeta2_2 = reshape(samples2.beta2, [numel(samples2.beta2),1]);
    allLambda_2 = reshape(samples2.lambda, [numel(samples2.lambda),1]);

    subplot(2,6,7);
    histogram(allAlpha_2, 'Normalization', 'probability');
    xlabel('alpha','FontSize', 10);
    ylabel('P(alpha)','FontSize', 10);
    set(gca,'FontSize',8);
    
    subplot(2,6,8);
    histogram(allBeta1_2, 'Normalization', 'probability');
    xlabel('beta1','FontSize', 10);
    ylabel('P(beta1)','FontSize', 10);
    set(gca,'FontSize',8);

    subplot(2,6,9);
    histogram(allBeta2_2, 'Normalization', 'probability');
    xlabel('beta2','FontSize', 10);
    ylabel('P(beta2)','FontSize', 10);
    set(gca,'FontSize',8);

    subplot(2,6,12);
    histogram(allLambda_2, 'Normalization', 'probability');
    xlabel('lambda','FontSize', 10);
    ylabel('P(lambda)','FontSize', 10);
    set(gca,'FontSize',8);

%     suptitle(sprintf('Subject %d', s));
    sgtitle(sprintf('Subject %d', s));
    orient landscape
    print(plotsFileName, '-dpsc2', '-append');

    allVariables_1 = [allAlpha_1, allBeta1_1, allBeta2_1, allBeta3_1, ...
        allBeta4_1, allLambda_1];
    varNames = {'alpha'; 'beta1'; 'beta2'; 'beta3'; 'beta4'; 'lambda'};
    corrplot(allVariables_1, 'varNames', varNames);
    fig = gcf;
    fig.PaperPositionMode = 'auto';
    print(plotsFileName, '-dpsc2', '-append');
    
    allVariables_2 = [allAlpha_2, allBeta1_2, allBeta2_2, allLambda_2];
    varNames = {'alpha'; 'beta1'; 'beta2'; 'lambda'};
    corrplot(allVariables_2, 'varNames', varNames);
	fig = gcf;
    fig.PaperPositionMode = 'auto';
    print(plotsFileName, '-dpsc2', '-append');
    
%     close all
end

