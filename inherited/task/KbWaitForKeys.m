function [response, responseTime, reactionTime, keyCode] = ...
    KbWaitForKeys(keys, waitTime)

response = false;
reactionTime = 0.0;
startTime = GetSecs();

while GetSecs() - startTime < waitTime
    [keyIsDown, responseTime, keyCode, ~] = KbCheck;
    if keyIsDown && any(keyCode(keys))
        reactionTime = responseTime - startTime;
        response = true;
        break;
    end
end

end