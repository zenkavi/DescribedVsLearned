close all
clear
clc

parcodes = [1 3:20 22:25 27];
numSubjects = length(parcodes);
numSessions = 5;
        
%% Load all experimental data.

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
subjs = [];
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
                subjs(trialCount) = p;
                session(trialCount) = s;
                subjTrial(trialCount) = trial;

                trialCount = trialCount + 1;
                totalNumTrials = totalNumTrials + 1;
            end
        end
    end
end


%% Fit Q learning model for each subject (free parameter: learning rate).

learningRate = zeros(numSubjects,1);
for p=parcodes
    % Get data for this subject.
    subjChoiceLeft = choiceLeft(subjs==p & responded==1);
    subjRewardLeft = rewardFractalLeft(subjs==p & responded==1);
    subjRewarRight = rewardFractalRight(subjs==p & responded==1);
    learningRate(p) = FitQLearning(subjChoiceLeft, subjRewardLeft, ...
        subjRewarRight);
end


%% Calculate Q values for all trials.

qLeft = zeros(totalNumTrials, 1);
qRight = zeros(totalNumTrials, 1);

currQLeft = 0;
currQRight = 0;

currentSubj = -1;
for numTrial = 1:totalNumTrials
    if subjs(numTrial) ~= currentSubj
        % Restart learning process.
        currentSubj = subjs(numTrial);
        alpha = learningRate(currentSubj);
        currQLeft = 0;
        currQRight = 0;
    end
    qLeft(numTrial) = currQLeft;
    qRight(numTrial) = currQRight;

    % Update current Q values.
    if responded(numTrial) == 1
        currQLeft = qLeft(numTrial) + ...
            alpha * (rewardFractalLeft(numTrial) - qLeft(numTrial));
        currQRight = qRight(numTrial) + ...
            alpha * (rewardFractalRight(numTrial) - qRight(numTrial));
    end
end


%% Write data to CSV file.

fileIdExp = fopen('bundles_data.csv', 'w+');
fprintf(fileIdExp, ['subject,session,trial_number,fractal_draw,' ...
    'prob_fractal_draw,prob_lottery,value_lottery,prob_fractal_left,' ...
    'prob_fractal_right,reward_fractal_left,reward_fractal_right,' ...
    'q_left,q_right,responded,choice_left,rt,outcome\n']);

currSubj = 1;
for i=1:totalNumTrials
    if learningRate(subjs(i)) ~= 0 && subjs(i) ~= currSubj
        currSubj = subjs(i);
    end
    if learningRate(subjs(i)) ~= 0
        fprintf(fileIdExp, ['%d,%d,%d,%d,%f,%f,%f,%f,%f,%d,%d,%f,%f,' ...
            '%d,%d,%f,%f\n'], currSubj, session(i), subjTrial(i), ...
            fractalDraw(i), probFractalDraw(i), lotteryProb(i), ...
            lotteryValue(i), probFractalLeft(i), probFractalRight(i), ...
            rewardFractalLeft(i), rewardFractalRight(i), qLeft(i), ...
            qRight(i), responded(i), choiceLeft(i), rt(i), reward(i));
    end
end

fclose(fileIdExp);
