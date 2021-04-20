function learningRate = FitQLearning(choices, rewardLeft, rewardRight)

T = 1;
alphaGrid = 0:0.05:1;
numTrials = length(choices);
NLLs = zeros(length(alphaGrid), 1);

% Notation: Q(trial, action).
% Actions: 1=right, 2=left.

i = 1;
for alpha = alphaGrid
    Q = zeros(numTrials, 2);
    for t = 2:numTrials
        Q(t,1) = Q(t-1,1) + alpha * (rewardRight(t-1) - Q(t-1,1));
        Q(t,2) = Q(t-1,2) + alpha * (rewardLeft(t-1) - Q(t-1,2));
    end
    
    for t = 1:numTrials
        likelihood = SoftMax(Q(t,:), choices(t), T);
        NLLs(i) = NLLs(i) - log(likelihood);
    end
    i = i + 1;
end

[~, idx] = min(NLLs);
learningRate = alphaGrid(idx);
end


function likelihood = SoftMax(Q, choice, T)
    s = 0;
    for i=1:length(Q)
        s = s + exp(Q(i)/T);
    end
    likelihood = exp(Q(choice+1)/T) / s;
end
