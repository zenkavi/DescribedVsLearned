function expdata = SetupSession(session, parcode, fractals)

%**************************************************************************
%       SESSION DATA STRUCTURE
%**************************************************************************
expdata = struct();
expdata.currentDir = pwd;
expdata.dataDir = [pwd '/expdata/'];
mkdir(expdata.dataDir);


%**************************************************************************
%       EXPERIMENT AND SESSION INFORMATION
%**************************************************************************
expdata.parcode = parcode;
expdata.fractals = fractals;
expdata.session = session;
expdata.fname = [expdata.dataDir 'expdata_' expdata.parcode '_s' ...
    num2str(expdata.session)];


%**************************************************************************
%       META-PARAMETERS
%**************************************************************************
expdata.scan = true;
expdata.debug = false;


%**************************************************************************
%       FMRI-PARAMETERS
%**************************************************************************
expdata.TR = 1;


%**************************************************************************
%       IMAGES
%**************************************************************************
expdata.imagesDir = [pwd '/images/'];


%**************************************************************************
%       SESSION PARAMETERS
%**************************************************************************
expdata.numTrials = 60;

expdata.referenceValue = 1;
expdata.referenceProb = 0.5;
expdata.values = [0.5, 2, 2.5, 5, ...
                  0.1, 1, 2, 10, ...
                  0.3, 1, 2, 3, ...
                  0.7, 1, 2, 7, ...
                  0.9, 1, 2, 9];
expdata.probs = [1, 0.25, 0.2, 0.1, ...
                 1, 0.1, 0.05, 0.01, ...
                 1, 0.3, 0.15, 0.1, ...
                 1, 0.7, 0.35, 0.1, ...
                 1, 0.9, 0.45, 0.1];
expdata.numLotteries = length(expdata.values);

expdata.probBundles = [zeros(5,1); ...
                       repmat(0.1,5,1); ...
                       repmat(0.2,5,1); ...
                       repmat(0.3,6,1); ...
                       repmat(0.4,6,1); ...
                       repmat(0.5,6,1); ...
                       repmat(0.6,6,1); ...
                       repmat(0.7,6,1); ...
                       repmat(0.8,5,1); ...
                       repmat(0.9,5,1); ...
                       ones(5,1)];
% expdata.probBundles = [zeros(100,1); ...
%                        repmat(0.1,15,1); ...
%                        repmat(0.2,10,1); ...
%                        repmat(0.3,10,1); ...
%                        repmat(0.4,10,1); ...
%                        repmat(0.5,10,1); ...
%                        repmat(0.6,10,1); ...
%                        repmat(0.7,10,1); ...
%                        repmat(0.8,10,1); ...
%                        repmat(0.9,15,1); ...
%                        ones(100,1)];

expdata.numRewardedTrials = 35;

expdata.randomWalkSigma = 0.025;
expdata.randomWalkLowBound= 0.25;
expdata.randomWalkUpBound= 0.75;

expdata.timeInstructions = 20;
expdata.timeRefLotteryPresentation = 4;
expdata.timeProbPresentation = 2;
expdata.timeRewardScreen = 3;

expdata.maxRT = 4;
expdata.possibleITIs = 4:7;


%**************************************************************************
%       SESSION DATA
%**************************************************************************
expdata.probFractalDraw = [];
expdata.lotteryValue =[];
expdata.lotteryProb = [];
expdata.fractalLeftProb = [];
expdata.fractalRightProb = [];
expdata.fixedITI = [];

% To be determined at each trial.
expdata.responseTime = [];
expdata.reactionTime = [];
expdata.responded = [];
expdata.choiceLeft = [];
expdata.currTrialRemainder = 0;
expdata.onsetCross = [];
expdata.onsetProbabilities = [];
expdata.onsetStimulus = [];
expdata.onsetReward = [];
expdata.reward = [];
expdata.ITI = [];
expdata.fractalDraw = [];
expdata.leftFractalReward = [];
expdata.rightFractalReward = [];


%**************************************************************************
%       DETERMINE ALL TRIAL PARAMETERS
%**************************************************************************
    % Determine probability for fractal draw in each trial.
    probFractalDraw = ...
        expdata.probBundles(randperm(length(expdata.probBundles)));
    expdata.probFractalDraw = probFractalDraw(1:expdata.numTrials);

    % Determine lotteries.
    lotteryValue = [];
    lotteryProb = [];
    for i=1:ceil(expdata.numTrials / expdata.numLotteries)
        lotteryValue = [lotteryValue; transpose(expdata.values)];
        lotteryProb = [lotteryProb; transpose(expdata.probs)];
    end
    lotteryValue = lotteryValue(1:expdata.numTrials);
    lotteryProb = lotteryProb(1:expdata.numTrials);
    idx = randperm(expdata.numTrials);
    expdata.lotteryValue = lotteryValue(idx);
    expdata.lotteryProb = lotteryProb(idx);

    % Determine reward probabilities for fractals.
    probLeft = expdata.randomWalkLowBound + ...
        (expdata.randomWalkUpBound - expdata.randomWalkLowBound) * rand;
	probRight = expdata.randomWalkLowBound + ...
        (expdata.randomWalkUpBound - expdata.randomWalkLowBound) * rand;
    for i=1:expdata.numTrials
        step = expdata.randomWalkSigma * randn;
        probLeft = probLeft + step;
        if probLeft > expdata.randomWalkUpBound
            probLeft = probLeft - (2 * abs(step));
        elseif probLeft < expdata.randomWalkLowBound
            probLeft = probLeft + (2 * abs(step));
        end
        step = expdata.randomWalkSigma * randn;
        probRight = probRight + step;
        if probRight > expdata.randomWalkUpBound
            probRight = probRight - (2 * abs(step));
        elseif probRight < expdata.randomWalkLowBound
            probRight = probRight + (2 * abs(step));
        end
        expdata.fractalLeftProb(i) = probLeft;
        expdata.fractalRightProb(i) = probRight;
    end

    % Determine all ITI fixed durations.
    ITIs = [];
    for i=1:ceil(expdata.numTrials / length(expdata.possibleITIs))
       ITIs = [ITIs; transpose(expdata.possibleITIs)];
    end
    ITIs = ITIs(1:expdata.numTrials);
    expdata.fixedITI = ITIs(randperm(length(ITIs)));
    % In the first trial, wait an additional 6 volumes to allow for scanner
    % equilibration.
    expdata.fixedITI(1) = 6 * expdata.TR;


%**************************************************************************
%       KEYBOARD
%**************************************************************************
% Enable unified mode of KbName, so KbName accepts identical key names on
% all operating systems.
KbName('UnifyKeyNames');
expdata.keySpace = KbName('space');
expdata.keyRight = KbName('3#');
expdata.keyLeft = KbName('2@');
expdata.choiceKeys = [expdata.keyLeft expdata.keyRight];


%**************************************************************************
%       INITIALIZE PTB
%**************************************************************************
global windowPtr
global oldVisualDebugLevel
global oldSupressAllWarnings

if expdata.debug == true
    screenSize = [0 0 640 400];
else
    screenSize = [];  % full screen
end

% If there are multiple displays guess that one without the menu bar is the
% best choice.  Dislay 0 has the menu bar.  
screens = Screen('Screens');
screenNumber = max(screens);
disp(screenNumber)
%screenNumber = 0;
% Open a window with two buffers.
nBuffers = 2;
[windowPtr, windowRect] = Screen('OpenWindow', screenNumber, ...
    [], screenSize, [], nBuffers);

% Remove the blue screen flash and minimize extraneous warnings.
if expdata.debug == true
    Screen('Preference', 'SkipSyncTests', 1);
    oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 4);
    oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 0);
else
    Screen('Preference', 'SkipSyncTests', 0);
    oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
    oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
    priorityLevel = MaxPriority(windowPtr);
    Priority(priorityLevel);  % set maximum priority level
    HideCursor(windowPtr);
end

% Enable alpha blending, so that the tranparency layer of images gets used.
Screen('BlendFunction', windowPtr, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

expdata.windowPtr = windowPtr;
expdata.windowRect = windowRect;


%**************************************************************************
%       COLORS
%**************************************************************************
white = WhiteIndex(expdata.windowPtr);
black = BlackIndex(expdata.windowPtr);
gray = [128, 128, 128];
red = [255 0 0];
expdata.colorCross = white;
expdata.colorBox = white;
expdata.colorPieChart = white;
expdata.colorText = white;                                                                                                             
expdata.colorBackground = black;
expdata.colorLine = gray;
expdata.colorChoiceBox = red;
expdata.colorReward = red;
Screen('FillRect', expdata.windowPtr, expdata.colorBackground);
Screen('TextColor', expdata.windowPtr, expdata.colorText);


%**************************************************************************
%       SHAPES
%**************************************************************************
expdata.lineWidth = 3;
expdata.choiceSize = 180;
expdata.choiceBoxSize = 180;
expdata.choiceBoxWidth = 10;


%**************************************************************************
%      DISPLAY PARAMETERS
%**************************************************************************
screenCenter = [floor((windowRect(1)+windowRect(3))/2) ...
    floor((windowRect(2)+windowRect(4))/2)];
expdata.screenCenter = screenCenter;
expdata.screenLowRight = [floor(screenCenter(1) + windowRect(3) / 4), ...
    floor(screenCenter(2) + (expdata.windowRect(4) / 5))];
expdata.screenLowLeft = [floor(screenCenter(1) - windowRect(3) / 4), ...
    floor(screenCenter(2) + (expdata.windowRect(4) / 5))];
expdata.screenUpRight = [floor(screenCenter(1) + windowRect(3) / 4), ...
    floor(screenCenter(2) - (expdata.windowRect(4) / 5))];
expdata.screenUpLeft = [floor(screenCenter(1) - windowRect(3) / 4), ...
    floor(screenCenter(2) - (expdata.windowRect(4) / 5))];

expdata.flipInterval = Screen('GetFlipInterval', expdata.windowPtr);


%**************************************************************************
%       TEXTURES
%**************************************************************************
[image, ~, ~] = ...
    imread([expdata.imagesDir int2str(expdata.fractals(1)) '.jpg']);
expdata.imageFractalLeft = Screen('MakeTexture', expdata.windowPtr, image);

[image, ~, ~] = ...
    imread([expdata.imagesDir int2str(expdata.fractals(2)) '.jpg']);
expdata.imageFractalRight = Screen('MakeTexture', expdata.windowPtr, image);

[img, ~, alpha] = ...
    imread([expdata.imagesDir 'reward_zero.png']);
img(:,:,4) = alpha(:,:);
expdata.imageRewardZero = Screen('MakeTexture', expdata.windowPtr, img);

[img, ~, alpha] = ...
    imread([expdata.imagesDir 'reward_one.png']);
img(:,:,4) = alpha(:,:);
expdata.imageRewardOne = Screen('MakeTexture', expdata.windowPtr, img);


%**************************************************************************
%       TEXT
%**************************************************************************
expdata.instructionTextSize = 26;
expdata.referenceTextSize = 40;
expdata.crossTextSize = 56;
expdata.probabilityTextSize = 64;
expdata.lotteryTextSize = 50;
expdata.messageTextSize = 46;
expdata.rewardTextSize = 68;
expdata.textFont = 'Arial';
Screen('TextFont', expdata.windowPtr, expdata.textFont);
Screen('TextSize', expdata.windowPtr, expdata.instructionTextSize);

end
