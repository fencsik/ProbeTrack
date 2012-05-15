function TrackDemo (nTargets, gapDur, gapType, probe)

% TrackDemo (NTARGETS, GAPDUR, GAPTYPE, PROBE)
%
% Runs MOT Demos
%
% NTARGETS is the number of targets to track out of 8 total stimuli.
% GAPDUR is the length of the "gap" in milliseconds.  GAPTYPE is one of: a
% = no gap, b = disappearance, c = small decrease in background brightness,
% and d = large increase in background brightness with a polarity reversal.
% PROBE specifies the post-gap probe delays in milliseconds; the program
% will pick one value at random for each tracking interval; with an empty
% array, there will be just an experimenter-controlled target-reveal at the
% end of the tracking interval.

% Author: David Fencsik (david.fencsik@csueastbay.edu)

    VERSION = '1.1';
    try
        AssertOpenGL;
        InitializePsychSound;
        KbName('UnifyKeyNames');

        % set any remaining control variables
        nStim = 8;
        nTrials = 100;

        % handle empty gaptype
        if (nargin < 3 || isempty(gapType))
            gapType = 'a';
        end

        % handle probe
        if (nargin < 4)
            probe = [];
        end
        if (~isempty(probe))
            probe = reshape(probe, [1, numel(probe)]);
        end

        % set up disappearance types
        dtNone = 0;
        dtBlank = 1;
        dtSmallFlash = 2;
        dtBigFlash = 3;
        switch gapType
          case 'a'
            gapType = dtNone;
          case 'b'
            gapType = dtBlank;
          case 'c'
            gapType = dtSmallFlash
          case 'd'
            gapType = dtBigFlash;
          otherwise
            error('Gap type "%s" not recognized', gapTypeCode);
        end

        % stimulus characteristics
        rectDisplay = [0 0 500 500];
        stimSize = 40;
        rectStim = [0 0 stimSize stimSize];

        % durations
        durCue = 60; % # of frames to present target cues
        durCueMove = 30; % # of frames of motion with cue
        durCueFade = 30; % # of frames of motion during which cue fades
        gapOnsetRange = [120 300]; % # of frames; bounds of range from which to pick pre-gap tracking duration
        durPostGap = 60;

        % define colors
        colCue = [255 255 0 255];
        colProbe = [255 0 0 255];
        colText = [255 255 255 255];
        colDisks = [125 125 125 255];
        colBackground = [72 72 72 255];
        colSmallFlash = [24 24 24 255];
        colBigFlash = [255 255 255 255];

        % define color sets for each phase of the trial
        trackingColors= repmat(colDisks', 1, nStim);
        cueingColors = trackingColors;
        cueingColors(:, 1:nTargets) = repmat(colCue', 1, nTargets);

        % Define response keys
        respAbort = KbName('ESCAPE');

        % Miscellaneous setup
        seed = 100 * sum(clock);
        rand('twister', seed);

        % Open and set-up main window
        Screen('Preference', 'SkipSyncTests', 0);
        Screen('Preference', 'VisualDebugLevel', 3);
        screenNumber=max(Screen('Screens'));
        [winMain, rectMain] = Screen('OpenWindow', screenNumber, 0, [], 32, 2);
        Screen(winMain, 'BlendFunction', GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        [centerX, centerY] = RectCenter(rectMain);

        % compute durations
        refreshDuration = Screen('GetFlipInterval', winMain);
        durSlack = refreshDuration / 2.0;
        % convert from ms -> frames
        gapDur = round(gapDur / 1000 / refreshDuration);
        if (~isempty(probe))
            probe = round(probe / 1000 / refreshDuration);
        end

        % Turn cursor and keyboard echoing off
        HideCursor;
        ListenChar(2);

        % font setup
        Screen('TextFont', winMain, 'Arial');
        Screen('TextSize', winMain, 18);

        for trial = 1:nTrials
            % pre-trial blank
            ClearScreen;
            Screen('Flip', winMain);

            % randomize gap onset (in frames)
            gapOnsetTime = Randi(gapOnsetRange(2) - gapOnsetRange(1)) + gapOnsetRange(1);
            % compute total trial duration (in frames)
            trialDuration = durCueMove + durCueFade + gapOnsetTime + gapDur + durPostGap;
            % select a probe delay and a probe object
            if (~isempty(probe))
                soa = probe(Randi(numel(probe)));
                trialDuration = trialDuration + soa;
                if (Randi(2) == 1)
                    % probe a target
                    probeIndex = Randi(nTargets);
                else
                    % probe a distractor
                    distractors = nTargets+1:nStim;
                    probeIndex = distractors(Randi(numel(distractors)));
                end
            end
            % compute stimulus positions for entire trial
            trajectories = MakeTrajectories(nStim, trialDuration, stimSize);

            % set colors for gap
            originalBackgroundColor = colBackground;
            gapColors = trackingColors;
            switch gapType
              case dtNone
                gapBackgroundColor = colBackground;
              case dtBlank
                gapBackgroundColor = colBackground;
                gapColors = repmat(gapBackgroundColor', 1, nStim);
              case dtSmallFlash
                gapBackgroundColor = colSmallFlash;
              case dtBigFlash
                gapBackgroundColor = colBigFlash;
            end

            % set colors for fading frames
            cueFadeColors = repmat(cueingColors, [1, 1, durCueFade]);
            cueFadeColors(4, :, :) = repmat(reshape(round(255:(-255 / (durCueFade - 1)):0), ...
                                                    [1, 1, durCueFade]), ...
                                            [1, nStim, 1]);

            % Reset suppression of keypress output on every trial, since Windows
            % intermittently resets suppression.
            ListenChar(2);

            % Draw plain display and wait a bit
            ClearScreen;
            PaintFrame(trajectories(:, :, 1), nStim, trackingColors, winMain);
            tLastOnset = Screen('Flip', winMain);
            targNextOnset = tLastOnset + .1;

            % Draw cue display and prompt for trial start
            ClearScreen;
            PaintFrame(trajectories(:, :, 1), nStim, cueingColors, winMain);
            KbReleaseWait();
            Screen('Flip', winMain, targNextOnset);
            [t, keyCode] = KbStrokeWait();
            if keyCode(respAbort)
                error('abort key pressed');
            end

            % Draw cue frame
            ClearScreen;
            PaintFrame(trajectories(:, :, 1), nStim, cueingColors, winMain);
            tLastOnset = Screen('Flip', winMain);
            targNextOnset = tLastOnset + durCue * refreshDuration - durSlack;
            KbReleaseWait();

            % main animation sequence
            % draw first post-cue frame
            frame = 1;
            ClearScreen;
            PaintFrame(trajectories(:, :, frame), nStim, cueingColors, winMain);
            Screen('Flip', winMain, targNextOnset);
            % cue + motion
            for f = 2:durCueMove
                frame = frame + 1;
                ClearScreen;
                PaintFrame(trajectories(:, :, frame), nStim, cueingColors, winMain);
                Screen('Flip', winMain);
            end
            % cue fade + motion
            for f = 1:durCueFade
                frame = frame + 1;
                ClearScreen;
                PaintFrame(trajectories(:, :, frame), nStim, trackingColors, winMain);
                PaintFrame(trajectories(:, :, frame), nStim, cueFadeColors(:, :, f), winMain);
                Screen('Flip', winMain);
            end
            currentColors = trackingColors;
            % pre-gap interval
            for f = 1:gapOnsetTime
                frame = frame + 1;
                ClearScreen;
                PaintFrame(trajectories(:, :, frame), nStim, currentColors, winMain);
                Screen('Flip', winMain);
            end
            colBackground = gapBackgroundColor;
            % gap interval
            for f = 1:gapDur
                frame = frame + 1;
                ClearScreen;
                PaintFrame(trajectories(:, :, frame), nStim, gapColors, winMain);
                Screen('Flip', winMain);
            end
            colBackground = originalBackgroundColor;
            if (~isempty(probe))
                % gap-probe soa
                for f = 1:soa
                    frame = frame + 1;
                    ClearScreen;
                    PaintFrame(trajectories(:, :, frame), nStim, currentColors, winMain);
                    Screen('Flip', winMain);
                end
                currentColors(:, probeIndex) = colProbe';
            end
            for f = 1:durPostGap
                % post-gap interval
                frame = frame + 1;
                ClearScreen;
                PaintFrame(trajectories(:, :, frame), nStim, currentColors, winMain);
                Screen('Flip', winMain);
            end

            % present final display
            ClearScreen;
            PaintFrame(trajectories(:, :, frame), nStim, currentColors, winMain);
            Screen('Flip', winMain);
            [t, keyCode] = KbStrokeWait();
            if keyCode(respAbort)
                error('abort key pressed');
            end

            % present target(s) again
            ClearScreen;
            currentColors(:, 1:nTargets) = cueingColors(:, 1:nTargets);
            PaintFrame(trajectories(:, :, frame), nStim, currentColors, winMain);
            Screen('Flip', winMain);
            [t, keyCode] = KbStrokeWait();
            if keyCode(respAbort)
                error('abort key pressed');
            end

            % Clear screen
            ClearScreenCompletely;
            tLastOnset = Screen('Flip', winMain);

        end % end trial loop
    catch
        ple;
    end

    Priority(0);
    ListenChar;
    ShowCursor;
    fclose('all');
    Screen('CloseAll');
    PsychPortAudio('Close');
    clear all;


function ClearScreen
    Screen('FillRect', winMain, colBackground);
end


function ClearScreenCompletely
    Screen('FillRect', winMain, colBackground);
end


function PaintFrame(coordinates, nStim, diskColors, window)
    Screen('DrawDots', window, coordinates(:, 1:nStim, :), stimSize, diskColors, [], 2);
%     for i = 1:nStim
%         placeRect = CenterRectOnPoint(rectStim, coordinates(i, 1), coordinates(i, 2));
%         screen(window, 'FillOval', diskColors(i, :), placeRect);
%     end
end

function trajectories = MakeTrajectories (nStim, nFrames, stimSize)

% generates MVT trajectories
% given the number of objects and frames, returns positions for each object for each frame
% code adapted from Jen Dimase's motPictMem, Justin Junge
% started 9/10/2004
% current 9/14/2004

%  Creates target Locations.
    trajectories = zeros(2, nStim, nFrames);

% coordinate system
    cellSize = round(rectDisplay(3:4)/7); % size of initial position grid cell

    [fieldRect, xOffset, yOffset] = CenterRect(rectDisplay, rectMain);

    x = 0:6;
    xloc = xOffset + cellSize(1) * x;
    yloc = yOffset + cellSize(2) * x;

    [gridy, gridx] = meshgrid(yloc, xloc);

    shufflegrid = randperm(49);
    shufflegrid = shufflegrid(1:nStim);
    trajectories(1, :, 1) = gridx(shufflegrid(:));
    trajectories(2, :, 1) = gridy(shufflegrid(:));

    % motion parameters
    repulsionPower = 10000; % not sure where the value comes from
    inertia = 1;
    pathchange = 1.5; % Determines Degree of Change on Motion Paths
    forcefieldDistance = 1.5 * stimSize; % 1.5 = Item Repulsion only occurs within a field around each shape 25% the size of the shape.

    initMotion = (-4:4) * pathchange;
    frameMotion = (-2:2) * inertia;
    rXmove = initMotion(Randi(9, [1, nStim]));
    rYmove = initMotion(Randi(9, [1, nStim]));

    for f = 2:nFrames
        aa = f-1;

        % Repulsion
        repel = ones(2, nStim) * 0.00001;

        for object=1:nStim;
            % this code is useless
            %             thisLocation = repmat(trajectories(object, :, aa), nStim, 1); % creates nStim X 2 array of object object's coordinates
            %             distances  = thisLocation - trajectories(:, :, aa);
            %             absoluteDistances = abs(distances);
            %             i = find((absoluteDistance(1) < forcefieldDistance) & (absoluteDistance(2) < forcefieldDistance));

            for qq=1:nStim;
                if qq ~= object
                    distance = trajectories(:, object, aa) - trajectories(:, qq, aa);
                    absoluteDistance = abs(distance);
                    if (absoluteDistance(1) < forcefieldDistance) && (absoluteDistance(2) < forcefieldDistance)
                        if distance(1) ~= 0 && distance(2) ~= 0
                            addrepel = 1./((distance.^2) .* sign(distance));
                            repel(:, object) = repel(:, object) + repulsionPower * addrepel;
                        end
                    end
                end
            end
        end

        rXchange = frameMotion(Randi(5, [1, nStim]));
        rYchange = frameMotion(Randi(5, [1, nStim]));
        newrm = [-1 1];

        % X Trajectories
        for object = 1:nStim
            rXmove(object) = rXmove(object) + rXchange(object) + repel(1, object);										% Makes change to X motion

            % floor and ceiling
            if rXmove(object)==0;
                rXmove(object) = newrm(Randi(2));
            end

            if rXmove(object) > 4*pathchange
                rXmove(object) = 4*pathchange;
            end
            if rXmove(object) < -4*pathchange
                rXmove(object) = -4*pathchange;
            end

            trajectories(1, object, f) = (trajectories(1, object, aa) + rXmove(object));

            if trajectories(1, object, f) >= (fieldRect(3) - stimSize); 		% Bounces off right
                trajectories(1, object, f) = (fieldRect(3) - stimSize);
                rXmove(object) = -(4*rXmove(object));
            end
            if  trajectories(1, object, f) <= fieldRect(1);					% Bounces off left
                trajectories(1, object, f) = fieldRect(1);
                rXmove(object) = -(4*rXmove(object));
            end
        end

        % Y Trajectories
        for object = 1:nStim

            rYmove(object) = rYmove(object) + rYchange(object) + repel(2, object);										% Makes change to Y motion

            if rYmove(object)==0;
                rYmove(object) = newrm(Randi(2));
            end

            if rYmove(object) > 4*pathchange
                rYmove(object) = 4*pathchange;
            end
            if rYmove(object) < -4*pathchange
                rYmove(object) = -4*pathchange;
            end

            trajectories(2, object, f) = (trajectories(2, object, aa) + rYmove(object));
            if trajectories(2, object, f) >= (fieldRect(4) - stimSize);													% Bounces off Bottom
                trajectories(2, object, f) = (fieldRect(4) - stimSize);
                rYmove(object) = -(4*rYmove(object));
            end

            if 	trajectories(2, object, f) <= fieldRect(2);																% Bounces off Top
                trajectories(2, object, f) = fieldRect(2);
                rYmove(object) = -(4*rYmove(object));
            end
        end
        %d(f, :) = sqrt(rXmove.^2 + rYmove.^2);
    end
    %mean(d)
end % end MakeTrajectories function

end
