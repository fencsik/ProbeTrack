## -*-Octave-*-

function ProbeTrackOct
    global par
    ## Runs MOT task with gap and variable post-gap probe-onset delay

    VERSION = "11.0-broken";
    try
        AssertOpenGL;
        InitializePsychSound;
        KbName("UnifyKeyNames");
        experiment = "ProbeTrackDT01";

        ## get user input
        [subject, blockType, pointsFlag] = ...
            GetExpInput("Subject code", [], "d", 0,
                        "Block type (1, 2, 3)", "3", "d", 0, ...
                        "Display points", "1", "d", 0);

        ## set any remaining IVs
        SOAlist = [0 1 2 4 75]; # # of frames
        probeTargetList = 0:1;
        nTargets = 3;

        ## set any remaining control variables
        durGap = 10; # # of frames
        nStim = 8;

        ## set up different block types
        switch blockType 
            case 1
                ## training without gap
                practiceFlag = 1;
                pTrials = 0;
                xTrials = 40;
                durGap = 0; # # of frames
                blockTypeStr = "NoGap";
                blockMesg = "Initial Block without Gap";
            case 2
                ## training with gap
                practiceFlag = 1;
                pTrials = 0;
                xTrials = 20;
                blockTypeStr = "GapPrac";
                blockMesg = "Training Block with Gap";
            case 3
                ## experimental block
                practiceFlag = 0;
                pTrials = 10;
                xTrials = 200;
                blockTypeStr = "GapExp";
                blockMesg = "Experimental Block with Gap";
            case -1
                practiceFlag = 0;
                pTrials = 2;
                xTrials = 8;
                blockTypeStr = "Testing";
                blockMesg = "Testing Run";
            otherwise
                error("Block type of %d not supported", blockType);
        endswitch
        totalTrials = xTrials + pTrials;

        ## stimulus characteristics
        par.rect.display = [0 0 500 500];
        par.stimSize = 40;
        par.rect.stim = [0 0 par.stimSize par.stimSize];

        ## durations
        durCue = 60; # # of frames to present target cues
        durCueMove = 38; # # of frames of motion with cue
        durCueFade = 38; # # of frames of motion during which cue fades
        durPostProbe = 60; # # of frames
        durFeedback = .746; # sec
        durPostTrialBlank = .5; # sec
        gapOnsetRange = [60 180]; # # of frames; bounds of range from which to pick pre-gap tracking duration
        gapOnsetRangeStr = sprintf("%d-%d", min(gapOnsetRange), max(gapOnsetRange));

        ## define colors
        par.col.black = [0 0 0 255];
        par.col.white = [255 255 255 255];
        par.col.midGray = [128 128 128 255];
        par.col.darkGray = [64 64 64 255];
        par.col.yellow = [240 240 0 255];
        par.col.red = [250 0 0 255];
        par.col.background = par.col.midGray;
        par.col.text = par.col.black;

        ## define color sets for each phase of the trial
        trackingColors= repmat(par.col.darkGray', 1, nStim);
        cueingColors = trackingColors;
        cueingColors(:, 1:nTargets) = repmat(par.col.yellow', 1, nTargets);
        gapColors = repmat(par.col.background', 1, nStim);

        ## Set any remaining parameters
        preloadFlag = 1;
        subjectPaced = 0; # does subject start each trial?
        pauseEvery = 4; # pause every N trials
        pauseMin = 4.0; # sec

        ## Define point setup: Add pointsCorrect for every correct response,
        ## subtract pointsError for every incorrect response, and subtract
        ## floor(pointsTimePenalty * RTmsec) on each correct
        ## response. Stores points across blocks/sessions in pointsFile.
        pointsCorrect = 1000;
        pointsError = 1000;
        pointsTimePenalty = .5;
        pointsFile = sprintf("Points-%s.mat", experiment);
        pointsFieldName = sprintf("s%04d", subject);
        ## open or initialize points file
        try
            load(pointsFile);
        catch
            pointsArray = struct();
        end_try_catch
        ## create record of subject points if it doesn't exist, then extract it
        if ~isfield(pointsArray, pointsFieldName)
            pointsArray.(pointsFieldName) = 0;
        endif
        points = pointsArray.(pointsFieldName);
        pointsRun = 0;

        ## Define response keys
        respAbort = KbName("ESCAPE");
        if IsOSX
            respTarget = KbName("'\"");
        elseif IsWin
            respTarget = KbName("'");
        else
            error("no keyboard mapping for this operating system");
        endif
        respDistractor  = KbName("a"); # a key (left-hand side)
        allowedResponses = [respTarget, respDistractor];

        ## Tones
        samplingRate = 44100;
        paBeep = PsychPortAudio("Open", [], [], 0, samplingRate, 1);
        paClick = PsychPortAudio("Open", [], [], 0, samplingRate, 1);
        paBuzz = PsychPortAudio("Open", [], [], 0, samplingRate, 1);
        sndBeep = MakeBeep(880, .1, samplingRate);
        PsychPortAudio("FillBuffer", paBeep, sndBeep);
        sndClick = MakeBeep(1000, .01, samplingRate);
        PsychPortAudio("FillBuffer", paClick, sndClick);
        sndBuzz = MakeBuzz(.1, samplingRate);
        PsychPortAudio("FillBuffer", paBuzz, sndBuzz);

        ## Miscellaneous setup
        seed = 100 * sum(clock);
        rand("twister", seed);
        dataFileName = sprintf("%s-%03d-data.txt", experiment, subject);
        [status, result] = system("echo $HOSTNAME");
        if exist("TestingRoom", "file")
            computer = TestingRoom;
        elseif status == 0
            computer = strtok(result, ".");
            computer = computer(isletter(computer)); # remove any spaces or newlines
        else
            computer = "unknown";
        endif
        revision = VERSION;
        blocktime = datestr(now, "yyyymmdd.HHMMSS");;

        ## Open and set-up main window
        Screen("Preference", "SkipSyncTests", 0);
        Screen("Preference", "VisualDebugLevel", 4);
        screenNumber=max(Screen("Screens"));
        [par.win.main, par.rect.main] = Screen("OpenWindow", screenNumber, 0, [], 32, 2);
        refreshDuration = Screen("GetFlipInterval", par.win.main);
        Screen(par.win.main, "BlendFunction", GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        [centerX, centerY] = RectCenter(par.rect.main);
        durSlack = refreshDuration / 2.0;

        ## Turn cursor and keyboard echoing off
        HideCursor;
        ListenChar(2);

        ## font setup
        Screen("TextFont", par.win.main, "Arial");
        Screen("TextSize", par.win.main, 18);

        ## create point window
        if pointsFlag
            [par.win.points, par.rect.points] = Screen("OpenOffscreenWindow", par.win.main, ...
                                             par.col.background, [0 0 250 40]);
            Screen("TextFont", par.win.points, "Arial");
            Screen("TextSize", par.win.points, 24);
            par.rect.points = OffsetRect(par.rect.points, 10, 10);
            GeneratePoints(points);
        endif

        ## present instructions
        Screen("FillRect", par.win.main, par.col.background);
        if pointsFlag, PresentPoints; endif
        DrawFormattedText(par.win.main, ...
                          [blockMesg, "\n\n\n", ...
                           "--------------------------------------------------\n\n\n"...
                           sprintf("Track %d targets out of %d total stimuli\n\n\n", nTargets, nStim), ...
                           "Respond when one disk flashes red\n\n" ...
                           "Press YES if the red disk is a target\n\n" ...
                           "Press NO if the red disk is NOT a target\n\n\n\n", ...
                           sprintf("Press any key to begin block of %d trials", totalTrials)], ...
                          "center", "center", par.col.text);
        KbReleaseWait;
        Screen("Flip", par.win.main);
        Screen("FillRect", par.win.main, par.col.background);
        if pointsFlag, PresentPoints; endif
        [keyTime, keyCode] = KbStrokeWait;
        if keyCode(respAbort)
            error("abort key pressed");
        endif
        Screen("Flip", par.win.main);

        ## initialize block-level DVs
        blockRT = zeros(totalTrials, 1) - 1;
        blockAcc = zeros(totalTrials, 1) - 1;

        subBlockList = 1:2;
        trialCounter = 0;
        for subBlock = subBlockList
            if subBlock == 1
                nTrials = pTrials;
                prac = 1;
            else
                nTrials = xTrials;
                prac = 0;
            endif
            if practiceFlag
                prac = 1;
            endif
            if nTrials <= 0, continue; endif

            ## balance independent variables
            n = ceil(nTrials / numel(SOAlist) / 2);
            [SOA, probeTarget] = ...
                BalanceFactors(n, 1, SOAlist, probeTargetList);

            if numel(SOA) ~= nTrials && prac == 0
                warning("unbalanced design in sublock %d", subBlock);
            endif

            for trial = 1:nTrials
                trialCounter = trialCounter + 1;
                trialtime = datestr(now, "yyyymmdd.HHMMSS");;

                ## pre-trial blank
                ClearScreen;
                if pointsFlag, PresentPoints; endif
                ## DrawFormattedText(par.win.main, "Configuring trial...", "center", "center", par.col.text);
                Screen("Flip", par.win.main);

                ## randomize gap duration (in frames)
                gapOnsetTime = Randi(gapOnsetRange(2) - gapOnsetRange(1)) + gapOnsetRange(1);
                ## compute total trial duration (in frames)
                trialDuration = durCueMove + durCueFade + gapOnsetTime + ...
                    durGap + SOA(trial) + durPostProbe;
                ## compute stimulus positions for entire trial
                trajectories = MakeTrajectories(nStim, trialDuration, par.stimSize);

                ## select probe
                if probeTarget(trial) == 1
                    ## probe a target
                    probeItem = Randi(nTargets);
                else
                    ## probe a distractor
                    probeItem = Randi(nStim - nTargets) + nTargets;
                endif

                ## set colors for probe frames
                probeColors = trackingColors;
                probeColors(:, probeItem) = par.col.red';

                ## set colors for fading frames
                cueFadeColors = repmat(cueingColors, [1, 1, durCueFade]);
                cueFadeColors(4, :, :) = repmat(reshape(round(255:(-255 / (durCueFade - 1)):0), ...
                                                        [1, 1, durCueFade]), ...
                                                [1, nStim, 1]);

                ## Set up timing variables
                tFrameOnset = zeros(size(trajectories, 3), 1) - 1;
                postProbeFrames = 0;
                probeOnsetTime = -1;
                response = -1;
                responseTime = -1;
                tResponseEnd = -1;

                ## Reset suppression of keypress output on every trial, since Windows
                ## intermittently resets suppression.
                ListenChar(2);

                ## Draw plain display and wait a bit
                ClearScreen;
                if pointsFlag, PresentPoints; endif
                PaintFrame(trajectories(:, :, 1), nStim, trackingColors, par.win.main);
                KbReleaseWait;
                tLastOnset = Screen("Flip", par.win.main);
                targNextOnset = tLastOnset + .1;

                ## Draw cue display and prompt for trial start
                if subjectPaced
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    PaintFrame(trajectories(:, :, 1), nStim, cueingColors, par.win.main);
                    DrawFormattedText(par.win.main, ...
                                      sprintf("Press a key to start trial %d", ...
                                              trialCounter), ...
                                      "center", "center", par.col.text);
                    KbReleaseWait;
                    Screen("Flip", par.win.main, targNextOnset);
                    [keyTime, keyCode] = KbStrokeWait;
                    if keyCode(respAbort)
                        error("abort key pressed");
                    endif
                endif

                ## Draw cue frame
                ClearScreen;
                if pointsFlag, PresentPoints; endif
                PaintFrame(trajectories(:, :, 1), nStim, cueingColors, par.win.main);
                tLastOnset = Screen("Flip", par.win.main);
                targNextOnset = tLastOnset + durCue * refreshDuration - durSlack;

                ## main animation sequence
                ## draw first post-cue frame
                frame = 1;
                ClearScreen;
                if pointsFlag, PresentPoints; endif
                PaintFrame(trajectories(:, :, frame), nStim, cueingColors, par.win.main);
                tFrameOnset(frame) = Screen("Flip", par.win.main, targNextOnset);
                ## cue + motion
                for f = 2:durCueMove
                    frame = frame + 1;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    PaintFrame(trajectories(:, :, frame), nStim, cueingColors, par.win.main);
                    tFrameOnset(frame) = Screen("Flip", par.win.main);
                endfor
                ## cue fade + motion
                for f = 1:durCueFade
                    frame = frame + 1;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    PaintFrame(trajectories(:, :, frame), nStim, trackingColors, par.win.main);
                    PaintFrame(trajectories(:, :, frame), nStim, cueFadeColors(:, :, f), par.win.main);
                    tFrameOnset(frame) = Screen("Flip", par.win.main);
                endfor
                ## pre-gap interval
                for f = 1:gapOnsetTime
                    frame = frame + 1;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    PaintFrame(trajectories(:, :, frame), nStim, trackingColors, par.win.main);
                    tFrameOnset(frame) = Screen("Flip", par.win.main);
                endfor
                for gLoop = 1:durGap
                    ## gap interval
                    frame = frame + 1;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    PaintFrame(trajectories(:, :, frame), nStim, gapColors, par.win.main);
                    tFrameOnset(frame) = Screen("Flip", par.win.main);
                endfor
                for sLoop = 1:SOA(trial)
                    ## SOA interval
                    frame = frame + 1;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    PaintFrame(trajectories(:, :, frame), nStim, trackingColors, par.win.main);
                    tFrameOnset(frame) = Screen("Flip", par.win.main);
                endfor
                ## present probe and continue motion while checking for a response every frame
                while response == -1
                    postProbeFrames = postProbeFrames + 1;
                    frame = frame + 1;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    if frame <= trialDuration
                        PaintFrame(trajectories(:, :, frame), nStim, probeColors, par.win.main);
                    endif
                    Screen("DrawingFinished", par.win.main);
                    [keyIsDown, KbTime, keyCode] = KbCheck;
                    if keyIsDown
                        if keyCode(respAbort)
                            error("abort key pressed");
                        endif
                        response = find(keyCode);
                        responseTime = KbTime;
                    endif
                    tLastOnset = Screen("Flip", par.win.main);
                    if frame <= numel(tFrameOnset)
                        tFrameOnset(frame) = tLastOnset;
                    endif
                    if probeOnsetTime < 0
                        probeOnsetTime = tLastOnset;
                    endif
                endwhile

                ## Wait for key release
                if response > 0
                    while any(keyCode(response))
                        [keyIsDown, KbTime, keyCode] = KbCheck;
                    endwhile
                    tResponseEnd = KbTime;
                endif

                ## compute RT and accuracy
                if responseTime > 0
                    RT = round((responseTime - probeOnsetTime) * 1000); # RT in ms
                    dur = round((tResponseEnd - responseTime) * 1000); # response dur in ms
                else
                    RT = 0;
                    dur = 0;
                endif
                if isempty(response)
                    ## no response
                    respString = "none";
                    acc = -1;
                elseif numel(response) > 1
                    ## multiple keys pressed
                    respString = "multi";
                    acc = -2;
                elseif response == respTarget
                    respString = "target";
                    if probeTarget(trial)
                        acc = 1;
                    else
                        acc = 0;
                    endif
                elseif response == respDistractor
                    respString = "distractor";
                    if probeTarget(trial)
                        acc = 0;
                    else
                        acc = 1;
                    endif
                else
                    ## some other key was pressed
                    respString = sprintf("%d", response);
                    acc = -3;
                endif

                ## compute durations
                durFrames = diff(tFrameOnset(tFrameOnset > 0)) * 1000;

                ## process points
                pointsTrial = 0;
                if acc <= 0
                    pointsTrial = -1 * pointsError;
                else
                    pointsTrial = max(pointsCorrect - floor(RT * pointsTimePenalty), 0);
                endif

                ## store trial info
                blockRT(trial) = RT;
                blockAcc(trial) = acc;
                points = points + pointsTrial;
                pointsRun = pointsRun + pointsTrial;
                pointsArray.(pointsFieldName) = points;
                if pointsFlag, GeneratePoints(points); endif

                ## output data
                dataFile = fopen(dataFileName, "r");
                if dataFile == -1
                    header = ["exp\tsub\tcode\trev\tcomp\truntime\ttrialtime\t" ...
                              "nstim\trefreshdur\tcuedur\tgapdur\tgapOnsetRange\tdurPostProbe\tgapOnsetTime\t" ...
                              "blocktype\tprac\ttrial\tntargets\tsoa\tprobeTarget\t" ...
                              "resp\tacc\trt\tpts\ttpts\tmeanFrameDur\tminFrameDur\tmaxFrameDur\n"];
                else
                    fclose(dataFile);
                    header = [];
                endif
                dataFile = fopen(dataFileName, "a");
                if dataFile == -1
                    error("cannot open data file %s for writing", dataFileName);
                endif
                if ~isempty(header)
                    fprintf(dataFile, header);
                endif
                fprintf(dataFile, ["%s\t%d\t%s\t%s\t%s\t%s\t%s\t%d\t%0.6f\t" ...
                                   "%d\t%d\t%s\t%d\t%d\t%s\t%d\t%d\t%d\t%d\t" ...
                                   "%d\t%s\t%d\t%d\t%d\t%d\t%0.3f\t%0.3f\t%0.3f\n"], ...
                        experiment, subject, mfilename, revision, computer, ...
                        blocktime, trialtime, nStim, refreshDuration, ...
                        durCue, durGap, gapOnsetRangeStr, durPostProbe, ...
                        gapOnsetTime, blockTypeStr, prac, trialCounter, ...
                        nTargets, SOA(trial), probeTarget(trial), respString, ...
                        acc, RT, pointsTrial, points, ...
                        mean(durFrames), min(durFrames), max(durFrames(2:end)));
                fclose(dataFile);

                ## Prepare feedback
                switch acc
                    case -1
                        feedback = "NO RESPONSE!";
                    case -2
                        feedback = "MULTIPLE KEYS PRESSED!";
                    case -3
                        feedback = "NON-RESPONSE KEY PRESSED!";
                    case 0
                        feedback = "ERROR";
                    case 1
                        feedback = "CORRECT";
                    otherwise
                        error("unknown accuracy code %d", acc);
                endswitch
                if acc >= 0
                    if pointsFlag
                        feedback = sprintf("TRIAL %d - %s\n\n\nPOINTS = %s", ...
                                           trialCounter, feedback, ...
                                           NumberWithSeparators(pointsTrial));
                    else
                        feedback = sprintf("TRIAL %d - %s\n\n\nResponse Time = %0.0f ms", ...
                                           trialCounter, feedback, RT);
                    endif
                endif

                ## Present feedback
                ClearScreenCompletely;
                if pointsFlag, PresentPoints; endif
                DrawFormattedText(par.win.main, feedback, "center", "center", par.col.text);
                tLastOnset = Screen("Flip", par.win.main);
                targNextOnset = tLastOnset + durFeedback - durSlack;
                ClearScreen;
                if pointsFlag, PresentPoints; endif
                tLastOnset = Screen("Flip", par.win.main, targNextOnset);
                targNextOnset = tLastOnset + durPostTrialBlank - durSlack;
                ClearScreen;
                if pointsFlag, PresentPoints; endif
                Screen("Flip", par.win.main, targNextOnset);

                ## pause every N trials, unless there's only one or no trials remaining
                if mod(trialCounter, pauseEvery) == 0 && ...
                        (totalTrials - trialCounter > 1)
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    DrawFormattedText(...
                                      par.win.main, "Please take a short break\n\n\n\n", ...
                                      "center", "center", par.col.text);
                    t1 = Screen("Flip", par.win.main);
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    DrawFormattedText(...
                                      par.win.main, ...
                                      ["Please take a short break\n\n\n\n", ...
                                       "Press any button to continue"], ...
                                      "center", "center", par.col.text);
                    Screen("Flip", par.win.main, t1 + pauseMin);
                    KbStrokeWait;
                    ClearScreen;
                    if pointsFlag, PresentPoints; endif
                    Screen("Flip", par.win.main);
                endif

            endfor # end trial loop

            ## output performance summary
            fprintf("\nBlock %d", subBlock);
            fprintf("\npcor  = %0.1f%%", 100 * mean(blockAcc(blockAcc >= 0)));
            fprintf("\nrtcor = %0.0f ms\n", mean(blockRT(blockAcc > 0)));
        endfor # end block loop

        ## prepare final screen
        closingString = sprintf("Overall accuracy = %0.0f%%\n\n", ...
                                100 * mean(blockAcc(blockAcc >= 0)));
        if pointsFlag
            closingString = sprintf("%sPoints Earned = %s", ...
                                    closingString, ...
                                    NumberWithSeparators(pointsRun));
        else
            closingString = sprintf("%sAverage response time = %0.0f ms", ...
                                    closingString, ...
                                    mean(blockRT(blockAcc > 0 & blockRT > 0)));
        endif
        ClearScreenCompletely;
        if pointsFlag, PresentPoints; endif
        DrawFormattedText(par.win.main, ...
                          ["Trial block is complete\n\n\n", ...
                           closingString, "\n\n\n", ...
                           "Please inform the experimenter that you are done."], ...
                          "center", "center", par.col.text);
        Screen("Flip", par.win.main);
        while 1
            [keyTime, keyCode] = KbStrokeWait;
            if keyCode(respAbort)
                break;
            endif
        endwhile
    catch
        ple;
    end_try_catch

    if exist("pointsArray", "var") && exist("pointsFile", "var")
        save(pointsFile, "pointsArray");
    endif
    Priority(0);
    ListenChar;
    ShowCursor;
    fprintf("\n# of open windows = %d\n", numel(Screen("Windows")));
    fclose("all");
    Screen("CloseAll");
    PsychPortAudio("Close");
    clear all;
endfunction

function GeneratePoints (pts)
    global par
    Screen("FillRect", par.win.points, par.col.background);
    DrawFormattedText(par.win.points, ...
                      sprintf("Points = %s", NumberWithSeparators(pts)), ...
                      [], [], par.col.text);
endfunction


function PresentPoints
    global par
    Screen("DrawTexture", par.win.main, par.win.points, [], par.rect.points);
endfunction


function s = NumberWithSeparators (n)
    n = int2str(floor(n)); # convert to string
    ln = numel(n); # number of digits in n
    if n(1) == "-"
        lc = floor((ln - 2) ./ 3); # number of commas (ignore -)
    else
        lc = floor((ln - 1) ./ 3); # number of commas
    endif
    s = repmat(" ", [1, ln + lc]);
    j = ln + lc;
    for i = 0:ln-1
        if i > 0 && n(ln-i) ~= "-" && mod(i, 3) == 0
            s(j) = ",";
            j = j - 1;
        endif
        s(j) = n(ln-i); j = j - 1;
    endfor
endfunction


function varargout = GetExpInput (varargin)
    n = nargin;
    if nargout ~= n / 4
        error("input and output arguments must match");
    endif
    prompt = varargin(1:4:n);
    defaultValues = varargin(2:4:n);
    inputType = varargin(3:4:n);
    confirmInput = varargin(4:4:n);
    n = numel(prompt);
    varargout = cell(1, nargout);
    for (i = 1:n)
        varargout{i} = GetInput(prompt{i}, defaultValues{i}, inputType{i},
                                confirmInput{i});
    endfor
endfunction

function response = GetInput (prompt, default, inputType, confirm)
    if (!isempty(default))
        df = sprintf(" [%s]", default);
    else
        df = "";
    endif
    prompt2 = sprintf("\n%s%s: ", prompt, df);
    do
        done = 1;
        response = input(prompt2, "s");
        if (isempty(response))
            if (isempty(default))
                done = 0;
            else
                response = default;
            endif
        endif
        if (!isempty(response))
            if (inputType != "s")
                [done, response] = ProcessResponse(response, inputType);
            endif
            if (confirm)
                done = ConfirmResponse(response);
            endif
        endif
    until (done)
endfunction

function [success, responseOut] = ProcessResponse(responseIn, rtype)
    if (any(rtype == "dif")) % match d/i = integer; f = float
        success = 0;
        [responseOut, n] = sscanf(responseIn, "%f");
        if (n == 1)
            success = 1;
        endif
        if (any(rtype == "di"))
            responseOut = fix(responseOut);
        endif
    else
        success = 1;
        responseOut = responseIn;
    endif
endfunction

function confirmed = ConfirmResponse(response)
    printf("You entered \"%s\".\nIs this correct? (y/n) ", response);
    do
        r = kbhit();
    until (any(r == "yYnN"))
    printf("%s\n", r);
    if (any(r == "yY"))
        confirmed = 1;
    else
        confirmed = 0;
    endif
endfunction

function [buzz, rate] = MakeBuzz (dur, rate)

    if nargin < 2 || isempty(rate) 
        rate = 44100;
    endif
    
    freqs = 100 * [1:15, 75:100];

    n = 0;
    buzz = zeros(1, length(0:rate*dur));
    for f = freqs
        buzz = buzz + sin(2 * pi * f * (0:rate*dur) / rate);
        n = n + 1;
    endfor
    buzz = buzz / n;
    buzz = (buzz - min(buzz)) / (max(buzz) - min(buzz)) * 2 - 1;
endfunction


function ClearScreen
    global par
    Screen("FillRect", par.win.main, par.col.background);
endfunction


function ClearScreenCompletely
    global par
    Screen("FillRect", par.win.main, par.col.background);
endfunction


function PaintFrame(coordinates, nStim, diskColors, window)
    global par
    Screen("DrawDots", window, coordinates(:, 1:nStim, :), par.stimSize, diskColors, [], 2);
    ##     for i = 1:nStim
    ##         placeRect = CenterRectOnPoint(par.rect.stim, coordinates(i, 1), coordinates(i, 2));
    ##         screen(window, "FillOval", diskColors(i, :), placeRect);
    ##     endfor
endfunction

function trajectories = MakeTrajectories (nStim, nFrames, stimSize)
    global par

    ## generates MVT trajectories
    ## given the number of objects and frames, returns positions for each object for each frame
    ## code adapted from Jen Dimase's motPictMem, Justin Junge
    ## started 9/10/2004
    ## current 9/14/2004

    ##  Creates target Locations.
    trajectories = zeros(2, nStim, nFrames);

    ## coordinate system
    cellSize = round(par.rect.display(3:4)/7); # size of initial position grid cell

    [fieldRect, xOffset, yOffset] = CenterRect(par.rect.display, par.rect.main);

    x = 0:6;
    xloc = xOffset + cellSize(1) * x;
    yloc = yOffset + cellSize(2) * x;

    [gridy, gridx] = meshgrid(yloc, xloc);

    shufflegrid = randperm(49);
    shufflegrid = shufflegrid(1:nStim);
    trajectories(1, :, 1) = gridx(shufflegrid(:));
    trajectories(2, :, 1) = gridy(shufflegrid(:));

    ## motion parameters
    repulsionPower = 10000; # not sure where the value comes from
    inertia = 1;
    pathchange = 1.5; # Determines Degree of Change on Motion Paths    for (i = 1:numel(prompt))
    forcefieldDistance = 1.5 * stimSize; # 1.5 = Item Repulsion only occurs within a field around each shape 25% the size of the shape. 

    initMotion = (-4:4) * pathchange;
    frameMotion = (-2:2) * inertia;
    rXmove = initMotion(Randi(9, [1, nStim]));
    rYmove = initMotion(Randi(9, [1, nStim]));

    for f = 2:nFrames
        aa = f-1;

        ## Repulsion 
        repel = ones(2, nStim) * 0.00001;

        for object=1:nStim;
            ## this code is useless
            ##             thisLocation = repmat(trajectories(object, :, aa), nStim, 1); # creates nStim X 2 array of object object's coordinates
            ##             distances  = thisLocation - trajectories(:, :, aa);
            ##             absoluteDistances = abs(distances);
            ##             i = find((absoluteDistance(1) < forcefieldDistance) & (absoluteDistance(2) < forcefieldDistance));

            for qq=1:nStim;
                if qq ~= object
                    distance = trajectories(:, object, aa) - trajectories(:, qq, aa);
                    absoluteDistance = abs(distance);
                    if (absoluteDistance(1) < forcefieldDistance) && (absoluteDistance(2) < forcefieldDistance)
                        if distance(1) ~= 0 && distance(2) ~= 0
                            addrepel = 1./((distance.^2) .* sign(distance));
                            repel(:, object) = repel(:, object) + repulsionPower * addrepel;
                        endif
                    endif
                endif
            endfor
        endfor

        rXchange = frameMotion(Randi(5, [1, nStim]));
        rYchange = frameMotion(Randi(5, [1, nStim]));
        newrm = [-1 1];

        ## X Trajectories
        for object = 1:nStim
            rXmove(object) = rXmove(object) + rXchange(object) + repel(1, object);										# Makes change to X motion

            ## floor and ceiling
            if rXmove(object)==0;
                rXmove(object) = newrm(Randi(2));
            endif

            if rXmove(object) > 4*pathchange
                rXmove(object) = 4*pathchange;
            endif
            if rXmove(object) < -4*pathchange
                rXmove(object) = -4*pathchange;
            endif

            trajectories(1, object, f) = (trajectories(1, object, aa) + rXmove(object));

            if trajectories(1, object, f) >= (fieldRect(3) - stimSize); 		# Bounces off right
                trajectories(1, object, f) = (fieldRect(3) - stimSize);
                rXmove(object) = -(4*rXmove(object));	
            endif
            if  trajectories(1, object, f) <= fieldRect(1);					# Bounces off left
                trajectories(1, object, f) = fieldRect(1);
                rXmove(object) = -(4*rXmove(object));											
            endif
        endfor

        ## Y Trajectories
        for object = 1:nStim

            rYmove(object) = rYmove(object) + rYchange(object) + repel(2, object);										# Makes change to Y motion

            if rYmove(object)==0;
                rYmove(object) = newrm(Randi(2));
            endif

            if rYmove(object) > 4*pathchange
                rYmove(object) = 4*pathchange;
            endif
            if rYmove(object) < -4*pathchange
                rYmove(object) = -4*pathchange;
            endif

            trajectories(2, object, f) = (trajectories(2, object, aa) + rYmove(object));							
            if trajectories(2, object, f) >= (fieldRect(4) - stimSize);													# Bounces off Bottom
                trajectories(2, object, f) = (fieldRect(4) - stimSize);
                rYmove(object) = -(4*rYmove(object));	
            endif

            if 	trajectories(2, object, f) <= fieldRect(2);																# Bounces off Top
                trajectories(2, object, f) = fieldRect(2);
                rYmove(object) = -(4*rYmove(object));									
            endif
        endfor
        ##d(f, :) = sqrt(rXmove.^2 + rYmove.^2);
    endfor
    ##mean(d)
endfunction
