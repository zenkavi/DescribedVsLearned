function FinalCleanUp

global oldVisualDebugLevel;
global oldSupressAllWarnings;

Screen('Preference', 'SkipSyncTests', 0);
Screen('Preference', 'VisualDebugLevel', oldVisualDebugLevel);
Screen('Preference', 'SuppressAllWarnings', oldSupressAllWarnings);
ShowCursor;
Priority(0);
Screen('CloseAll');
    
end
