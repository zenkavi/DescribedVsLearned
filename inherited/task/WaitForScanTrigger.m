function expdata = WaitForScanTrigger(expdata)
    FlushEvents('keyDown');
    done = 0;
    while done == 0
        av = CharAvail();
        if av ~= 0
            if str2double(GetChar()) == 5
                done = 1;
                expdata.startTime = GetSecs();
            end
        end
    end
end