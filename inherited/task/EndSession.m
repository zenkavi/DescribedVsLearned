function EndSession(expdata)
global numSessions

% End-of-session screen.
if expdata.session == numSessions
    correctedReward = EndExperiment(expdata.totalReward);
    Screen('FillRect', expdata.windowPtr, expdata.colorBackground);
    text = 'All done! Thank you for participating.';
    Screen('TextSize', expdata.windowPtr, expdata.instructionTextSize);
    DrawFormattedText(expdata.windowPtr, text, 'center', ...
        'center', expdata.colorText, [], [], [], 2);
    Screen('Flip', expdata.windowPtr);
else
    correctedReward = -1;
    Screen('FillRect', expdata.windowPtr, expdata.colorBackground);
    text = sprintf(['End of session %d!\nPlease wait for the ' ...
                    'experimenter...'], expdata.session);
    Screen('TextSize', expdata.windowPtr, expdata.instructionTextSize);
    DrawFormattedText(expdata.windowPtr, text, 'center', ...
        'center', expdata.colorText, [], [], [], 2);
    Screen('Flip', expdata.windowPtr);
end

% Delete unnecessary variables.
fieldsToDelete = {'windowPtr', 'imagesDir', 'keySpace', 'keyLeft', ...
    'keyRight', 'choiceKeys', 'colorCross', 'colorBox', ...
    'colorChoiceBox', 'colorPieChart', 'colorText', 'colorBackground', ...
    'colorLine', 'colorReward', 'lineWidth', 'choiceSize', ...
    'imageFractalLeft', 'imageFractalRight', 'imageRewardZero', ...
    'imageRewardOne', 'instructionTextSize', 'referenceTextSize', ...
    'crossTextSize', 'probabilityTextSize', 'lotteryTextSize', ...
    'rewardTextSize', 'messageTextSize', 'textFont'};
expdata = rmfield(expdata, fieldsToDelete);

% Save data.
expdata.endTimeSerial = now;
expdata.endTime = datestr(expdata.endTimeSerial,30);
SaveWithoutOverwrite([expdata.fname '_' expdata.endTime], expdata);

WaitSecs(40);
disp('End of session.');
if correctedReward > 0
    fprintf('Amount earned: %f\n', correctedReward);
end
% Clean up.
FinalCleanUp;

end