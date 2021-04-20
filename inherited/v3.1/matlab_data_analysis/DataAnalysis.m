close all
clear
clc

colors = distinguishable_colors(30);
cyan = colors(11,:);
pink = colors(5,:);
green = colors(13,:);
purple = colors(16,:);
magenta = colors(15,:);
yellow = colors(6,:);

transparency = 0;

% exportDataToCsv = true;
exportDataToCsv = false;


trialReward = 1;

valueDiffsCompare = -4:2:4;
valueDiffs = -0.4:0.2:0.4;

weightedValueDiffs = [-0.4:0.04:-0.24, ...
                      -0.2:0.02:0, ...
                      0.02:0.02:0.2, ...
                      0.24:0.04:0.4];
weightedValueDiffsCompare = [-40:4:-24, ...
                             -20:2:0, ...
                             2:2:20, ...
                             24:4:40];

numSessions = 5;

parcodes = [1 3:20 22:25 27];
numSubjects = length(parcodes);

plotsFileBaseName = 'behavioral';
clocknum = clock;
dateString = [];
for i=1:5
    dateString = [dateString '_' num2str(clocknum(i))];
end
plotsFileName = [plotsFileBaseName dateString];


%% Load all experimental data.

lotteryValue = [];
lotteryProb = [];
expValLotteryRef = [];
expValLotteryShown = [];

probFractalLeft = [];
probFractalRight = [];

probFractalDraw = [];
probLotteryDraw = [];
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
data_path = '/Users/zeynepenkavi/Dropbox/RangelLab/DescribedVsLearned/v3.1/matlab_data_analysis';
for p=parcodes
    for s=1:numSessions
        if p < 10
            f = sprintf('%s/expdata/expdata_AR-GT-BUNDLES-0%d_s%d_*.mat', ...
                data_path, p, s);
        else
            f = sprintf('%s/expdata/expdata_AR-GT-BUNDLES-%d_s%d_*.mat', ...
                data_path, p, s);
        end
        files = dir(f);
        load(sprintf('expdata/%s', files(1).name));

        for trial = 1:data.numTrials
            if data.responded(trial) == 1
                lotteryValue(trialCount) = data.lotteryValue(trial);
                lotteryProb(trialCount) = data.lotteryProb(trial);
                expValLotteryRef(trialCount) = ...
                    data.referenceProb * data.referenceValue;
                expValLotteryShown(trialCount) = ...
                    data.lotteryProb(trial) * data.lotteryValue(trial);

                probFractalLeft(trialCount) = data.fractalLeftProb(trial);
                probFractalRight(trialCount) = ...
                    data.fractalRightProb(trial);

                probFractalDraw(trialCount) = data.probFractalDraw(trial);
                probLotteryDraw(trialCount) = ...
                    1 - probFractalDraw(trialCount);
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

mean(learningRate(parcodes))
std(learningRate(parcodes))


%% Calculate Q values for all trials.

qLeft = zeros(totalNumTrials, 1);
qRight = zeros(totalNumTrials, 1);

currQLeft = 0;
currQRight = 0;

currentSubj = -1;
for trial = 1:totalNumTrials
    if subjs(trial) ~= currentSubj
        % Restart learning process.
        currentSubj = subjs(trial);
        alpha = learningRate(currentSubj);
        currQLeft = 0;
        currQRight = 0;
    end
    qLeft(trial) = currQLeft;
    qRight(trial) = currQRight;

    % Update current Q values.
    if responded(trial) == 1
        currQLeft = qLeft(trial) + ...
            alpha * (rewardFractalLeft(trial) - qLeft(trial));
        currQRight = qRight(trial) + ...
            alpha * (rewardFractalRight(trial) - qRight(trial));
    end
end


%% Write data to CSV file.

if exportDataToCsv
    fileIdExp = fopen('bundles_data.csv', 'w+');
    fprintf(fileIdExp, ['subject,session,trial_number,fractal_draw,' ...
        'prob_fractal_draw,prob_lottery,value_lottery,' ...
        'prob_fractal_left,prob_fractal_right,reward_fractal_left,' ...
        'reward_fractal_right,q_left,q_right,responded,choice_left,rt,' ...
        'outcome\n']);

    currSubj = 1;
    for i=1:totalNumTrials
        if learningRate(subjs(i)) ~= 0 && subjs(i) ~= currSubj
            currSubj = subjs(i);
        end
        if learningRate(subjs(i)) ~= 0
            fprintf(fileIdExp, ['%d,%d,%d,%d,%f,%f,%f,%f,%f,%d,%d,%f,' ...
                '%f,%d,%d,%f,%f\n'], currSubj, session(i), ...
                subjTrial(i), fractalDraw(i), probFractalDraw(i), ...
                lotteryProb(i), lotteryValue(i), probFractalLeft(i), ...
                probFractalRight(i), rewardFractalLeft(i), ...
                rewardFractalRight(i), qLeft(i), qRight(i), ...
                responded(i), choiceLeft(i), rt(i), reward(i));
        end
    end

    fclose(fileIdExp);
end


%% Check whether response in each trial was correct or incorrect.

correct = [];
for trial = 1:totalNumTrials
    valueLeft = (probFractalDraw(trial) * qLeft(trial)) + ...
        ((1 - probFractalDraw(trial)) * expValLotteryShown(trial));
    valueRight = (probFractalDraw(trial) * qRight(trial)) + ...
        ((1 - probFractalDraw(trial)) * expValLotteryRef(trial));
    
    if valueLeft > valueRight
        if choiceLeft(trial) == 1
            correct = [correct; 1];
        else
            correct = [correct; 0];
        end
    elseif valueLeft < valueRight
        if choiceLeft(trial) == 1
            correct = [correct; 0];
        else
            correct = [correct; 1];
        end
    else
        correct = [correct; 1];
    end
end


%% Sanity check for learning rates: psychometric choice curve as a function
% of fractal Q values, for all trials where probability of fractal draw is
% >= 70%.

figure;

bins = -1:0.2:1;
step = 0.1;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if (1 - probLotteryDraw(i)) >= 0.7
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);

shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
title('Choice vs. fractal Q values (trials with P_{gamble} <= 30%)', ...
    'FontSize', 18);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('Q_{left} - Q_{right}','FontSize', 18);
ylabel('P(left)','FontSize', 18);
set(gca,'FontSize',12);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Psychometric choice curve as a function of gamble expected values.

figure;
countLeft = zeros(length(valueDiffs),1);
countTotal = zeros(length(valueDiffs),1);
for i=1:length(choiceLeft)
    valueDiff = int8(10 * (expValLotteryShown(i) - expValLotteryRef(i)));
    idx = find(valueDiffsCompare==valueDiff);
    countTotal(idx) = countTotal(idx) + 1;
    countLeft(idx) = countLeft(idx) + choiceLeft(i);
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);

subplot(2,2,1);
shadedErrorBar(valueDiffs, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([-0.5 0.5],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 1]);
title('Choice vs. gamble expected values');
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of fractal Q values.

bins = -1:0.2:1;
step = 0.1;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    valueDiff = qLeft(i) - qRight(i);
    for bin=bins
        if valueDiff >= bin - step && valueDiff < bin + step
            break
        end
    end
    idx = find(bins==bin);
    countTotal(idx) = countTotal(idx) + 1;
    countLeft(idx) = countLeft(idx) + choiceLeft(i);
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);

subplot(2,2,2);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
title('Choice vs. fractal Q values');
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of gamble expected values
% weighted by probability of gamble draw.

countLeft = zeros(length(weightedValueDiffs),1);
countTotal = zeros(length(weightedValueDiffs),1);
for i=1:length(choiceLeft)
    weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
        (expValLotteryShown(i) - expValLotteryRef(i)));
    
    idx = find(weightedValueDiffsCompare==weightedValueDiff);
    countTotal(idx) = countTotal(idx) + 1;
    countLeft(idx) = countLeft(idx) + choiceLeft(i);
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);

subplot(2,2,3);
shadedErrorBar(weightedValueDiffs, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', purple}, transparency);
line([-0.5 0.5],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 1]);
title('Choice vs. weighted gamble expected values');
xlabel('P_{gamble} * (EV_{left} - EV_{right})','FontSize', 14);
ylabel('P(left)','FontSize', 14);
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of fractal Q values weighted by
% probability of fractal draw.

bins = -1:0.1:1;
step = 0.05;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
    for bin=bins
        if valueDiff >= bin - step && valueDiff < bin + step
            break
        end
    end
    idx = find(bins==bin);
    countTotal(idx) = countTotal(idx) + 1;
    countLeft(idx) = countLeft(idx) + choiceLeft(i);
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);

subplot(2,2,4);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
title('Choice vs. weighted fractal Q values');
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('(1 - P_{gamble}) * (Q_{left} - Q_{right})','FontSize', 14);
ylabel('P(left)','FontSize', 14);
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Reaction times as a function of gamble expected values.

figure;

rts = cell(length(valueDiffs),1);
for i=1:length(rt)
    valueDiff = int8(10 *(expValLotteryShown(i) - expValLotteryRef(i)));
    idx = find(valueDiffsCompare==valueDiff);
    rts{idx} = [rts{idx}; rt(i)];
end
confInts = zeros(length(valueDiffs),1);
means = zeros(length(valueDiffs),1);
for i=1:length(valueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end

subplot(2,2,1);
shadedErrorBar(valueDiffs, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 2]);
title('RT vs. gamble expected values');
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
set(gca,'FontSize',10);


%% Reaction times as a function of fractal Q values.

bins = -1:0.2:1;
step = 0.1;
rts = cell(length(bins),1);
for i=1:totalNumTrials
    valueDiff = qLeft(i) - qRight(i);
    for bin=bins
        if valueDiff >= bin - step && valueDiff < bin + step
            break
        end
    end
    idx = find(bins==bin);
    rts{idx} = [rts{idx}; rt(i)];
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end

subplot(2,2,2);
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 2]);
title('RT vs. fractal expected values');
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
set(gca,'FontSize',10);


%% Reaction times as a function of gamble expected values weighted by
% probability of gamble draw.

rts = cell(length(weightedValueDiffs),1);
for i=1:length(rt)
    weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
        (expValLotteryShown(i) - expValLotteryRef(i)));
    idx = find(weightedValueDiffsCompare==weightedValueDiff);
    rts{idx} = [rts{idx}; rt(i)];
end
confInts = zeros(length(weightedValueDiffs),1);
means = zeros(length(weightedValueDiffs),1);
for i=1:length(weightedValueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end

subplot(2,2,3);
shadedErrorBar(weightedValueDiffs, means, confInts, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', purple}, transparency);
line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 2]);
title('RT vs. weighted gamble expected values');
xlabel('P_{gamble} * (EV_{left} - EV_{right})','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
set(gca,'FontSize',10);


%% Reaction times as a function of fractal Q values weighted by probability
% of fractal draw.

bins = -1:0.1:1;
step = 0.05;
rts = cell(length(bins),1);
for i=1:totalNumTrials
    valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
    for bin=bins
        if valueDiff >= bin - step && valueDiff < bin + step
            break
        end
    end
    idx = find(bins==bin);
    rts{idx} = [rts{idx}; rt(i)];
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end

subplot(2,2,4);
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 2]);
title('RT vs. weighted fractal expected values');
xlabel('(1 - P_{gamble}) * (Q_{left} - Q_{right})','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Psychometric choice curve as a function of gamble expected values,
% congruent vs. incongruent trials.

figure;

subplot(2,2,1); hold on;
countLeft = zeros(length(valueDiffs),1);
countTotal = zeros(length(valueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(valueDiffs, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

countLeft = zeros(length(valueDiffs),1);
countTotal = zeros(length(valueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(valueDiffs, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);
line([-0.5 0.5],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 1]);
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. gamble expected values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of fractal Q values, congruent
% vs. incongruent trials.

bins = -1:0.2:1;
step = 0.1;

subplot(2,2,2); hold on;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. fractal Q values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of gamble expected values
% weighted by probability of gamble draw, congruent vs. incongruent
% trials.

subplot(2,2,3); hold on;
countLeft = zeros(length(weightedValueDiffs),1);
countTotal = zeros(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(weightedValueDiffs, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', purple}, transparency);

countLeft = zeros(length(weightedValueDiffs),1);
countTotal = zeros(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(weightedValueDiffs, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', green}, transparency);
line([-0.5 0.5],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 1]);
xlabel('P_{gamble} * (EV_{left} - EV_{right})','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. weighted gamble expected values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of fractal Q values weighted by
% probability of fractal draw,  congruent vs. incongruent trials.

bins = -0.8:0.2:0.8;
step = 0.1;

subplot(2,2,4); hold on;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
xlabel('(1 - P_{gamble}) * (Q_{left} - Q_{right})','FontSize', 14);
title('Choice vs. weighted fractal Q values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Reaction times as a function of gamble expected values, congruent vs.
% incongruent trials.

figure;
subplot(2,2,1); hold on;

rts = cell(length(valueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(valueDiffs),1);
means = zeros(length(valueDiffs),1);
for i=1:length(valueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(valueDiffs, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

rts = cell(length(valueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(valueDiffs),1);
means = zeros(length(valueDiffs),1);
for i=1:length(valueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(valueDiffs, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 2]);
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. gamble expected values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of fractal Q values, congruent vs.
% incongruent trials.

subplot(2,2,2); hold on;
bins = -1:0.2:1;
step = 0.1;

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 2]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. fractal Q values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of gamble expected values weighted by
% probability of lottery draw, congruent vs. incongruent trials.

subplot(2,2,3); hold on;

rts = cell(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(weightedValueDiffs),1);
means = zeros(length(weightedValueDiffs),1);
for i=1:length(weightedValueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(weightedValueDiffs, means, confInts, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', purple}, transparency);

rts = cell(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(weightedValueDiffs),1);
means = zeros(length(weightedValueDiffs),1);
for i=1:length(weightedValueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(weightedValueDiffs, means, confInts, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', green}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 2]);
xlabel('P_{gamble} * (EV_{left} - EV_{right})','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. weighted gamble expected values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of fractal Q values weighted by probability
% of fractal draw, congruent vs. incongruent trials.

subplot(2,2,4); hold on;
bins = -0.8:0.1:0.8;
step = 0.05;

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 2]);
xlabel('(1 - P_{gamble}) * (Q_{left} - Q_{right})','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. weighted fractal Q values');
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Psychometric choice curve as a function of gamble expected values,
% correct vs. incorrect choice.

figure;

subplot(2,2,1); hold on;
countLeft = zeros(length(valueDiffs),1);
countTotal = zeros(length(valueDiffs),1);
for i=1:totalNumTrials
    if correct(i)
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(valueDiffs, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

countLeft = zeros(length(valueDiffs),1);
countTotal = zeros(length(valueDiffs),1);
for i=1:totalNumTrials
    if ~ correct(i)
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(valueDiffs, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);
line([-0.5 0.5],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 1]);
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. gamble expected values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of fractal Q values, correct vs.
% incorrect choice.

bins = -1:0.2:1;
step = 0.1;

subplot(2,2,2); hold on;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if correct(i)
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if ~ correct(i)
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. fractal Q values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of gamble expected values
% weighted by probability of gamble draw, correct vs. incorrect choice.

subplot(2,2,3); hold on;
countLeft = zeros(length(weightedValueDiffs),1);
countTotal = zeros(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if correct(i)
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(weightedValueDiffs, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', cyan}, transparency);

countLeft = zeros(length(weightedValueDiffs),1);
countTotal = zeros(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if ~ correct(i)
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(weightedValueDiffs, probLeft, confInt, {'-', 'Marker', ...
    '.', 'MarkerSize', 14, 'Color', magenta}, transparency);
line([-0.5 0.5],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 1]);
xlabel('P_{gamble} * (EV_{left} - EV_{right})','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. weighted gamble expected values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of fractal Q values weighted by
% probability of fractal draw, correct vs. incorrect choice.

bins = -1:0.1:1;
step = 0.05;

subplot(2,2,4); hold on;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if correct(i)
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:totalNumTrials
    if ~ correct(i)
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([-1.05 1.05],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 1]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
xlabel('(1 - P_{gamble}) * (Q_{left} - Q_{right})','FontSize', 14);
title('Choice vs. weighted fractal Q values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Reaction times as a function of gamble expected values, correct vs.
% incorrect choice.

figure;
subplot(2,2,1); hold on;

rts = cell(length(valueDiffs),1);
for i=1:totalNumTrials
    if correct(i)
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(valueDiffs),1);
means = zeros(length(valueDiffs),1);
for i=1:length(valueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(valueDiffs, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

rts = cell(length(valueDiffs),1);
for i=1:totalNumTrials
    if ~ correct(i)
        valueDiff = int8(10 * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(valueDiffsCompare==valueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(valueDiffs),1);
means = zeros(length(valueDiffs),1);
for i=1:length(valueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(valueDiffs, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 2]);
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. gamble expected values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of fractal Q values, correct vs.
% incorrect choice.

subplot(2,2,2); hold on;
bins = -1:0.2:1;
step = 0.1;

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if correct(i)
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if ~ correct(i)
        valueDiff = qLeft(i) - qRight(i);
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 2]);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. fractal Q values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of gamble expected values weighted by
% probability of gamble draw, correct vs. incorrect choice.

subplot(2,2,3); hold on;

rts = cell(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if correct(i)
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(weightedValueDiffs),1);
means = zeros(length(weightedValueDiffs),1);
for i=1:length(weightedValueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(weightedValueDiffs, means, confInts, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', cyan}, transparency);

rts = cell(length(weightedValueDiffs),1);
for i=1:totalNumTrials
    if ~ correct(i)
        weightedValueDiff = int8(100 * probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)));
        idx = find(weightedValueDiffsCompare==weightedValueDiff);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(weightedValueDiffs),1);
means = zeros(length(weightedValueDiffs),1);
for i=1:length(weightedValueDiffs)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(weightedValueDiffs, means, confInts, {'-', 'Marker', ...
    '.', 'MarkerSize', 14, 'Color', magenta}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-0.5 0.5]);
ylim([0 2]);
xlabel('P_{gamble} * (EV_{left} - EV_{right})','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. weighted gamble expected values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of fractal Q values weighted by probability
% of fractal draw, correct vs. incorrect choice.

subplot(2,2,4); hold on;
bins = -0.8:0.2:0.8;
step = 0.1;

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if correct(i)
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

rts = cell(length(bins),1);
for i=1:totalNumTrials
    if ~ correct(i)
        valueDiff = probFractalDraw(i) * (qLeft(i) - qRight(i));
        for bin=bins
            if valueDiff >= bin - step && valueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1.05 1.05]);
ylim([0 2]);
xlabel('(1 - P_{gamble}) * (Q_{left} - Q_{right})','FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
title('RT vs. weighted fractal Q values');
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Psychometric choice curve as a function of total weighted value
% difference.

figure;
% suptitle('Choice vs. total weighted value difference');
sgtitle('Choice vs. total weighted value difference');

bins = -1:0.2:1;
step = 0.1;
countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:length(choiceLeft)
    diffLottery = probLotteryDraw(i) * ...
        (expValLotteryShown(i) - expValLotteryRef(i));
    diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
    weightedValueDiff = diffLottery + diffFractal;
    
    for bin=bins
        if weightedValueDiff >= bin - step && ...
                weightedValueDiff < bin + step
            break
        end
    end
    idx = find(bins==bin);
    countTotal(idx) = countTotal(idx) + 1;
    countLeft(idx) = countLeft(idx) + choiceLeft(i);
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);

subplot(1,3,1);
shadedErrorBar(bins, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', purple}, transparency);
line([-1 1],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1 1]);
ylim([0 1]);
%xlabel(['P_{gamble} * (EV_{L} - EV_{R}) + (1 - P_{gamble}) ' ...
%    '* (Q_{L} - Q_{R})'],'FontSize', 14);
ylabel('P(left)','FontSize', 14);
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of total weighted value
% difference, correct vs. incorrect choice.

subplot(1,3,2); hold on;
bins = -1:0.2:1;
step = 0.1;

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:length(choiceLeft)
    if correct(i)
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        
        if bin==bins(end) && choiceLeft(i)==0
            disp(i);
        end
        
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', cyan}, transparency);

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:length(choiceLeft)
    if ~ correct(i)
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([-1 1],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1 1]);
ylim([0 1]);
xlabel(['P_{gamble} * (EV_{L} - EV_{R}) + (1 - P_{gamble}) ' ...
    '* (Q_{L} - Q_{R})'],'FontSize', 14);
%ylabel('P(left)','FontSize', 14);
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Psychometric choice curve as a function of total weighted value
% difference, congruent vs. incongruent trials.

subplot(1,3,3); hold on;
bins = -1:0.2:1;
step = 0.1;

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:length(choiceLeft)
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        
        if bin==bins(end) && choiceLeft(i)==0
            disp(i);
        end
        
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, ...
    {'-', 'Marker', '.', 'MarkerSize', 14, 'Color', purple}, transparency);

countLeft = zeros(length(bins),1);
countTotal = zeros(length(bins),1);
for i=1:length(choiceLeft)
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        countTotal(idx) = countTotal(idx) + 1;
        countLeft(idx) = countLeft(idx) + choiceLeft(i);
    end
end
probLeft = countLeft ./ countTotal;
stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
shadedErrorBar(bins, probLeft, confInt, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([-1 1],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5]);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1 1]);
ylim([0 1]);
%xlabel(['P_{gamble} * (EV_{L} - EV_{R}) + (1 - P_{gamble}) ' ...
%    '* (Q_{L} - Q_{R})'],'FontSize', 14);
%ylabel('P(left)','FontSize', 14);
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Reaction times as a function of total weighted value difference.

figure;
% suptitle('RT vs. total weighted value difference');
sgtitle('RT vs. total weighted value difference');

bins = -1:0.2:1;
step = 0.1;
rts = cell(length(bins),1);

for i=1:length(rt)
    diffLottery = probLotteryDraw(i) * ...
        (expValLotteryShown(i) - expValLotteryRef(i));
    diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
    weightedValueDiff = diffLottery + diffFractal;
    
    for bin=bins
        if weightedValueDiff >= bin - step && ...
                weightedValueDiff < bin + step
            break
        end
    end
    idx = find(bins==bin);
    rts{idx} = [rts{idx}; rt(i)];
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end

subplot(1,3,1);
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);
line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1 1]);
ylim([0 2]);
%xlabel(['P_{gamble} * (EV_{L} - EV_{R}) + (1 - P_{gamble}) ' ...
%    '* (Q_{L} - Q_{R})'],'FontSize', 14);
ylabel('Reaction time (s)','FontSize', 14);
set(gca,'FontSize',10);


%% Reaction times as a function of total weighted value difference, correct
% vs. incorrect choice.

subplot(1,3,2); hold on;
bins = -1:0.2:1;
step = 0.1;

rts = cell(length(bins),1);
for i=1:length(rt)
    if correct(i)
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', cyan}, transparency);

rts = cell(length(bins),1);
for i=1:length(rt)
    if ~correct(i)
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', magenta}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1 1]);
ylim([0 2]);
xlabel(['P_{gamble} * (EV_{L} - EV_{R}) + (1 - P_{gamble}) ' ...
    '* (Q_{L} - Q_{R})'],'FontSize', 14);
%ylabel('Reaction time (s)','FontSize', 14);
legend('Correct choice', 'Incorrect choice', 'Location', 'SouthEast');
set(gca,'FontSize',10);


%% Reaction times as a function of total weighted value difference,
% congruent vs. incongruent trials.

subplot(1,3,3); hold on;
bins = -1:0.2:1;
step = 0.1;

rts = cell(length(bins),1);
for i=1:length(rt)
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) > 0
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', purple}, transparency);

rts = cell(length(bins),1);
for i=1:length(rt)
    if (qLeft(i) - qRight(i)) * ...
            (expValLotteryShown(i) - expValLotteryRef(i)) < 0
        diffLottery = probLotteryDraw(i) * ...
            (expValLotteryShown(i) - expValLotteryRef(i));
        diffFractal = probFractalDraw(i) * (qLeft(i) - qRight(i));
        weightedValueDiff = diffLottery + diffFractal;

        for bin=bins
            if weightedValueDiff >= bin - step && ...
                    weightedValueDiff < bin + step
                break
            end
        end
        idx = find(bins==bin);
        rts{idx} = [rts{idx}; rt(i)];
    end
end
confInts = zeros(length(bins),1);
means = zeros(length(bins),1);
for i=1:length(bins)
    confInts(i) = Get95PercConfidenceInterval(std(rts{i}), length(rts{i}));
    means(i) = mean(rts{i});
end
shadedErrorBar(bins, means, confInts, {'-', 'Marker', '.', ...
    'MarkerSize', 14, 'Color', green}, transparency);

line([0 0],[0 2],'LineStyle','--','Color',[0.5 0.5 0.5]);
xlim([-1 1]);
ylim([0 2]);
%xlabel(['P_{gamble} * (EV_{L} - EV_{R}) + (1 - P_{gamble}) ' ...
%    '* (Q_{L} - Q_{R})'],'FontSize', 14);
%ylabel('Reaction time (s)','FontSize', 14);
legend('Congruent trials', 'Incongruent trials', 'Location', 'SouthEast');
set(gca,'FontSize',10);

orient landscape
print('-dpsc2', '-append', plotsFileName);


%% Psychometric choice curves as a function of gamble expected values,
% grouped by the probability of a gamble draw.

figure;
subplot(1,2,1);
hold on;

probs = cell(5,1);
probs{1} = 0;
probs{2} = 0.1:0.1:0.3;
probs{3} = 0.4:0.1:0.6;
probs{4} = 0.7:0.1:0.9;
probs{5} = 1;

c = 1;
for p=1:5
    countLeft = zeros(length(valueDiffs),1);
    countTotal = zeros(length(valueDiffs),1);
    for i=1:totalNumTrials
        for currProb=probs{p}
            if abs(probLotteryDraw(i) - currProb) < 0.01
                valueDiff = int8(10 * ...
                    (expValLotteryShown(i) - expValLotteryRef(i)));
                idx = find(valueDiffsCompare==valueDiff);
                countTotal(idx) = countTotal(idx) + 1;
                countLeft(idx) = countLeft(idx) + choiceLeft(i);
            end
        end
    end
    probLeft = countLeft ./ countTotal;
    stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
    confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
    errorbar(valueDiffs, probLeft, confInt, '-', 'Marker', '.', ...
        'MarkerSize', 14, 'Color', colors(c,:), 'LineWidth', 2);
    c = c + 1;
end

line([-0.5 0.6],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5], ...
    'LineWidth', 2);
line([0 0],[0 1],'LineStyle','--','Color',[0.5 0.5 0.5], ...
    'LineWidth', 2);
xlim([-0.5 0.6]);
ylim([0 1]);
legend('0','0.1-0.3','0.4-0.6','0.7-0.9','1', 'Location', 'SouthEast');
set(gca,'FontSize',10);
xlabel('EV_{left} - EV_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);
title('Choice vs. EV diff grouped by prob of gamble draw', ...
    'FontSize', 10);


%% Psychometric choice curves as a function of fractal Q values, grouped
% by the probability of a fractal draw.

subplot(1,2,2);
hold on;
bins = -1:0.2:1;
step = 0.2;

probs = cell(5,1);
probs{1} = 0;
probs{2} = 0.1:0.1:0.3;
probs{3} = 0.4:0.1:0.6;
probs{4} = 0.7:0.1:0.9;
probs{5} = 1;

c = 1;
for p=1:5
    countLeft = zeros(length(bins),1);
    countTotal = zeros(length(bins),1);
    for i=1:totalNumTrials
        for currProb=probs{p}
            if abs(probFractalDraw(i) - currProb) < 0.01
                valueDiff = qLeft(i) - qRight(i);
                for bin=bins
                    if valueDiff >= bin - step && valueDiff < bin + step
                        break
                    end
                end
                idx = find(bins==bin);
                countTotal(idx) = countTotal(idx) + 1;
                countLeft(idx) = countLeft(idx) + choiceLeft(i);
            end
        end
    end
    probLeft = countLeft ./ countTotal;
    stdLeft = sqrt(probLeft .* (1-probLeft) ./ countTotal);
    confInt = Get95PercConfidenceInterval(stdLeft, countTotal);
    errorbar(bins, probLeft, confInt, '-', 'Marker', '.', ...
        'MarkerSize', 14, 'Color', colors(c,:), 'LineWidth', 2);
    c = c + 1;
end

line([-1.2 1.3],[0.5 0.5],'LineStyle','--','Color',[0.5 0.5 0.5], ...
    'LineWidth', 2);
line([0 0],[-0.05 1.05],'LineStyle','--','Color',[0.5 0.5 0.5], ...
    'LineWidth', 2);
xlim([-1.2 1.3]);
ylim([-0.05 1.05]);
legend('0','0.1-0.3','0.4-0.6','0.7-0.9','1', 'Location', 'SouthEast');
set(gca,'FontSize',10);
title('Choice vs. Q diff grouped by prob of fractal draw', ...
    'FontSize', 10);
xlabel('Q_{left} - Q_{right}','FontSize', 14);
ylabel('P(left)','FontSize', 14);

orient landscape
print('-dpsc2', '-append', plotsFileName);
% close all

