function RunSession(session)
global parcode
global fractals

try

%**************************************************************************
%       PTB INITIALIZATION/PARAMETER SETUP
%**************************************************************************
    expdata = SetupSession(session, parcode, fractals);


%**************************************************************************
%       INSTRUCTIONS
%**************************************************************************

    if session == 1
        disp('Showing instructions...');
        Screen('FillRect', expdata.windowPtr, expdata.colorBackground);
        text = ['You will now be presented with a sequence of trials.\n' ...
            'In each trial you will have to choose between two\n' ...
            'pairs, one on the left and one on the right. Each pair\n' ...
            'contains one fractal and one lottery. The fractals will\n' ...
            'be shown at the top of the screen. Each fractal is\n' ...
            'associated with a different probability (or chance) of\n' ...
            'receiving $1. The lotteries will be shown at the bottom\n' ...
            'of the screen. The lottery on the left will be\n' ...
            'different in each trial, while the lottery on the right\n' ...
            'will always be the reference lottery. Before each trial\n' ...
            'begins, you will see the probability (or chance) that\n' ...
            'the reward for that trial will be drawn from either the\n' ...
            'fractals or the lotteries. In each trial, press 2 to\n' ...
            'select the left option and 3 to select the right\n' ...
            'option; you will have a maximum of 4 seconds to respond.'];
        Screen('TextSize', expdata.windowPtr, expdata.instructionTextSize);
        DrawFormattedText(expdata.windowPtr, text, 'center', 'center', ...
            expdata.colorText);
        Screen('Flip', expdata.windowPtr);
        WaitSecs(expdata.timeInstructions);
    end


%**************************************************************************
%       REFERENCE LOTTERY PRESENTATION
%**************************************************************************
    disp('Reference lottery presentation...');
    Screen('FillRect', expdata.windowPtr, expdata.colorBackground);
    Screen('TextSize', expdata.windowPtr, expdata.referenceTextSize);
    text = 'This is your reference lottery:';
    DrawFormattedText(expdata.windowPtr, text, 'center', ...
        expdata.screenCenter(2) - floor(expdata.windowRect(4)/3), ...
        expdata.colorText);
    Screen('FillArc', expdata.windowPtr, expdata.colorPieChart, ...
        [expdata.screenCenter(1) - 80, expdata.screenCenter(2) - 120, ...
        expdata.screenCenter(1) + 80, expdata.screenCenter(2) + 40], ...
        0, ConvertProbabilityToPieChartAngle(expdata.referenceProb));
        text = sprintf('$%.2f', expdata.referenceValue);
    DrawFormattedText(expdata.windowPtr, text, 'center', ...
        expdata.screenCenter(2) + 100, expdata.colorText);
    Screen('Flip', expdata.windowPtr);
    WaitSecs(expdata.timeRefLotteryPresentation);


%**************************************************************************
%       SCANNER INITIALIZATION
%**************************************************************************
    % If in scanner, wait the appropriate time before starting task.
    if expdata.scan
        % Display prepare sign.
        Screen('TextSize', expdata.windowPtr, expdata.messageTextSize);
        Screen('FillRect', expdata.windowPtr, expdata.colorBackground);
        DrawFormattedText(expdata.windowPtr, 'Prepare to begin...', ...
            'center', 'center', expdata.colorText);
        Screen('Flip', expdata.windowPtr);
        % Register and wait for scan trigger ('5').
        expdata = WaitForScanTrigger(expdata);
    else
        expdata.startTime = GetSecs();
    end


%**************************************************************************
%       CHOICE TRIALS
%**************************************************************************
    for trial = 1:expdata.numTrials
        expdata = RunTrial(expdata, trial);
    end


%**************************************************************************
%       REWARD CALCULATION FOR THIS SESSION
%**************************************************************************
    shuffledIdxTrials = (randperm(expdata.numTrials));
    shuffledIdxTrials = shuffledIdxTrials(1:expdata.numRewardedTrials);
    expdata.totalReward = sum(expdata.reward(shuffledIdxTrials));


%**************************************************************************
%       END SESSION
%**************************************************************************
    EndSession(expdata);

catch
    FinalCleanUp;
    save(['task_error_dump_' datestr(now, 30)]);
    disp('*** RunSession code threw error, session did not complete. ***');
    psychrethrow(psychlasterror);

end  % try...catch
end