function correctedReward = EndExperiment(lastSessionReward)
global parcode
global numSessions
global minReward
global maxReward

% Total reward calculation for the experiment.
totalReward = lastSessionReward;
for session=1:numSessions-1
    files = dir(sprintf('expdata/expdata_%s_s%d_*.mat', parcode, session));
    load(sprintf('expdata/%s', files(1).name));
    totalReward = totalReward + data.totalReward;
end

if totalReward < minReward
    correctedReward = minReward;
elseif totalReward > maxReward
    correctedReward = maxReward;
else
    correctedReward = totalReward;
end
end
