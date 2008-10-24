function ProbeTrack

% Runs MOT task with gap and variable post-gap probe-onset delay

% $LastChangedDate$

% declare global variables -- would be better if we don't need these
% global MainWindow display
% global screenRect displayRect objectRect
% global white black midGray darkgray yellow red

try
    AssertOpenGL;
    InitializePsychSound;
    KbName('UnifyKeyNames');
    experiment = 'ProbeTrack7';

    % define rects
    displayRect = [0 0 500 500];
    objectSize = 40;
    objectRect = [0 0 objectSize objectSize];

    % define experimental variables
    % practiceTrials = 5;
    % experimentalTrials = 5;
    % nObjects = 8;
    % nTargets = 4;
    % cueDuration = 60; % time cue is up
    % gapDuration = 10; % duration of invisible motion
    % SOAlist = [0 6 12 24 96]; % time of probe from end of gap
    % gapOnsetRange = [60 180]; % time (in frames) that the gap begins, from onset of trial
    % postProbeDuration = 60; % period of motion after appearance of probe

    % Define response keys
    respAbort = KbName('ESCAPE');
    if IsOSX
        respTarget = KbName('''"');
    elseif IsWin
        respTarget = KbName('''');
    else
        error('no keyboard mapping for this operating system');
    end        
    respDistractor  = KbName('a'); % a key (left-hand side)
    allowedResponses = [respTarget, respDistractor];
    %%keys = [KbName('''')  KbName('a')]; % response key assignments


    % get user input
    [subject, practiceTrials, experimentalTrials, nObjects, nTargets, ...
     cueDuration, gapDuration, SOAlist, gapOnsetRange, postProbeDuration] = ...
        DialogBox('Experiment Parameters', ...
                  'Subject code:', '1', 1, ...
                  'practice trials'          , '16', 1, ...
                  'experimental trials'      , '120', 1, ...
                  'number of objects'        , '8', 1, ...
                  'number of targets'        , '4', 1, ...
                  'cue duration (frames)'    , '60', 1, ...
                  'gap duration'             , '10', 1, ...
                  'SOAs'                     , '0 3 6 96', 1, ...
                  'gap onset range (frames)' , '60 180', 1, ...
                  'post-probe duration'      , '60', 1);

    % define colors
    colBlack = [0 0 0];
    colMidGray = [128 128 128];
    colDarkGray = [64 64 64];
    colYellow = [240 240 0];
    colRed = [250 0 0];

    % now define color sets for each phase of the trial
    trackingColors= repmat(colDarkGray, nObjects, 1);
    cueingColors = trackingColors;
    cueingColors(1:nTargets, :) = repmat(colYellow, nTargets, 1);
    gapColors = repmat(colMidGray, nObjects, 1);

    % Open and set-up main window
    Screen('Preference', 'SkipSyncTests', 0);
    Screen('Preference', 'VisualDebugLevel', 4);
    screenNumber=max(Screen('Screens'));
    [winMain, rectMain] = Screen('OpenWindow', screenNumber, 0, [], 32, 2);
    refreshDuration = Screen('GetFlipInterval', winMain);
    Screen(winMain, 'BlendFunction', GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    [centerX, centerY] = RectCenter(rectMain);
    durSlack = refreshDuration / 2.0;

    % Turn cursor and keyboard echoing off
    HideCursor;
    ListenChar(2);

    % define colors
    colBlack = BlackIndex(winMain);
    colWhite = WhiteIndex(winMain);

    % miscellaneous setup
    dataFileName = [experiment, 'Data-', subject, '.txt'];
    rand('state', 100 * sum(clock));

    % setup data file
    % format for data output
    headerFormatString	=	'%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n';
    dataFormatString = 		'%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n';
    if exist(dataFileName, 'file') == 0
	dataFile = fopen(dataFileName, 'a');
	fprintf(dataFile, headerFormatString, 'sub', 'hz',  'nobjects', 'ntargets', 'cuedur', 'gapdur', 'soa', 'min_gapOnset', 'max_gapOnset', 'postProbeDuration', 'block', 'trial', 'gapOnset', 'probeType', 'error', 'badkey', 'rt');
	fclose(dataFile);
    end

    % present instructions
    HideCursor;

    Instructions = {
        'In this experiment, you will be asked to track a number of moving disks.';
        ['Each trial will start with a display of ',  num2str(nObjects) ' gray disks.'];
        'When you are ready, you will press a key to start the trial.';
        [num2str(nTargets), ' of the disks will change to yellow.'];
        'After a brief interval, these target disks will go back to gray and start to move.';
        'You will track the target disks for several seconds.';
        'During this time, the disks may briefly disappear from the screen.';
        'At a randomly chosen time, one of the disks will turn red';
        ['Your job is to indicate whether this probe disk was one of the ', num2str(nTargets), ' target disks you were tracking.'];
        'Press the ''quote'' key for "yes" and the ''a'' key for "no".';
        'Please respond as quickly and accurately as possible.'
        ['There will be ', num2str(practiceTrials), ' practice trials, followed by ', num2str(experimentalTrials), ' experimental trials.'];
                   };

    Screen('TextFont', winMain, 'Arial');
    Screen('TextSize', winMain, 24);

    Screen('FillRect', winMain, colMidGray);
    CenterCellText(winMain, Instructions, 30);
    CenterText('Press any key to continue', 0, 300);
    KbReleaseWait;
    Screen('Flip', winMain);
    KbStrokeWait;

    % block routine
    for block = 1:2
	if block == 1
            nTrials = practiceTrials;
            message = 'practice';
	else
            nTrials = experimentalTrials;
            message = 'experimental';
	end

	% set trial variables for the block
        ivs = {'SOA'        , SOAlist;
               'probeType'  , 1:2;
              };

        nVariables = size(ivs, 1);
        varLength = zeros(nVariables, 1);
        listLength = 1;
        for v = 1:nVariables
            varLength(v) = length(ivs{v, 2});
            listLength = listLength * varLength(v);
        end
        nRepetitions = ceil(nTrials / listLength);
        len1 = listLength;
        len2 = 1;
        [dummy, index] = sort(rand(listLength * nRepetitions, 1)); 
        for v = 1:nVariables
            len1 = len1 / varLength(v);
            eval([ivs{v, 1} ' = repmat(reshape(repmat(ivs{v, 2}, len1, len2), listLength, 1), nRepetitions, 1);']);
            eval([ivs{v, 1} ' = ' ivs{v, 1} '(index);']);
            len2 = len2 * varLength(v);
        end
        clear dummy len1 len2 index ivs varLength;

        if listLength * nRepetitions ~= nTrials
            warning('unbalanced design');
        end
        
        % set any remaining randomized variables
	gapOnsetTime = randi(gapOnsetRange(2) - gapOnsetRange(1), [nTrials, 1]) + gapOnsetRange(1); % sets random gap duration for all trials
        
	screen(winMain, 'FillRect', colMidGray);
	CenterText(['Press any key to start ', num2str(nTrials), ' ', message, ' trials']);
        KbStrokeWait;
	
	% trial routine
	screen(winMain, 'FillRect', colMidGray);
	CenterText('configuring trial...', 0, 0);

        % variables for storing RT and accuracy across trials
        blockRT = zeros(nTrials, 1);
        blockAcc = zeros(nTrials, 1);

	for trial = 1:nTrials
            trialDuration = gapOnsetTime(trial) + gapDuration + SOA(trial) + postProbeDuration; % duration of trial in frames
            trajectories = makeTrajectories(nObjects, trialDuration, objectSize); % gets item positions for all frames
            
            if probeType(trial) == 1
                % probe a target
                probeItem = randi(nTargets);
            else
                % probe a distractor
                probeItem = randi(nObjects - nTargets) + nTargets;
            end
            
            probeColors = trackingColors;
            probeColors(probeItem, :) = colRed; % set colors for probe frame
            
            % now put up initial display, wait for keypress to initiate trial, then cue targets
            screen(cueFrame, 'FillRect', colMidGray);
            screen(display, 'FillRect', colMidGray);
            paintFrame(trajectories(:, :, 1), nObjects, trackingColors, display);
            paintFrame(trajectories(:, :, 1), nObjects, cueingColors, cueFrame);
            
            screen(winMain, 'WaitBlanking');
            screen('CopyWindow', display, winMain);
            CenterText(['Press a key to start trial ', num2str(trial)], 0, 300);
            Screen('Flip', winMain);
            KbStrokeWait;
            screen(winMain, 'WaitBlanking');
            screen('CopyWindow', cueFrame, winMain);
            screen(winMain, 'WaitBlanking', cueDuration);
            
            postProbeFrames = 0;
            response = -1;
            % motion sequence
            for frame =1:gapOnsetTime(trial)
                % pre-gap interval
                paintFrame(trajectories(:, :, frame), nObjects, trackingColors, winMain);
                Screen('Flip', winMain);
            end
            for gLoop = 1:gapDuration
                % gap interval
                frame = frame + 1;
                paintFrame(trajectories(:, :, frame), nObjects, gapColors, winMain);
                Screen('Flip', winMain);
            end
            for sLoop = 1:SOA(trial)
                % SOA interval
                frame = frame + 1;
                paintFrame(trajectories(:, :, frame), nObjects, trackingColors, winMain);
                Screen('Flip', winMain);
            end			
            probeOnsetTime = GetSecs;
            % this next bit of code looks for a response in between refreshes
            responseTime = 0;
            while postProbeFrames < postProbeDuration && response == -1
                postProbeFrames = postProbeFrames + 1;
                frame = frame + 1;
                paintFrame(trajectories(:, :, frame), nObjects, probeColors, winMain);
                Screen('DrawingFinished', winMain);
                [keyIsDown, KbTime, keyCode] = KbCheck;
                if keyIsDown
                    response = find(keyCode);
                    response = response(1);
                    responseTime = KbTime;
                end
                screen('Flip', winMain);
            end
            if responseTime > 0
                RT = round((responseTime - probeOnsetTime) * 1000); % RT in ms
            else
                RT = 0;
            end

            % now classify response
            responseKey = find(allowedResponses == response);
            wrongKeyFlag = 0;
            if isempty(responseKey)
                error = 1;
                feedback = 'Wrong Key! Use "a" for "no" and "quote" for "yes"!';
                wrongKeyFlag = 1;
            elseif responseKey == probeType(trial)
                error = 0;
                feedback = 'Correct!';
            else
                error = 1;
                feedback = 'Wrong!';
            end

            screen(winMain, 'FillRect', colMidGray);
            CenterText([feedback, ' - RT = ', num2str(RT), ' ms']);
            WaitSecs(1);

            % save data
            dataFile = fopen(dataFileName, 'a');
            fprintf(dataFile, dataFormatString, subject, hz,  nObjects, nTargets, cueDuration, gapDuration, SOA(trial), gapOnsetRange(1), gapOnsetRange(2), postProbeDuration, block, trial, gapOnsetTime(trial), probeType(trial), error, wrongKeyFlag, RT);
            fclose(dataFile);

            % store trial info
            blockRT(trial) = RT;
            blockAcc(trial) = 1 - error;
            if wrongKeyFlag
                blockAcc(trial) = -1;
            end
	end % end trial loop

        % output performance summary
        fprintf('\nBlock %d', block);
        fprintf('\npcor  = %0.4f', mean(blockAcc(blockAcc >= 0)));
        fprintf('\nrtcor = %0.1f ms\n', mean(blockRT(blockAcc > 0)));

        screen(winMain, 'FillRect', colMidGray);
        CenterText('Thank you for participating!');
        CenterText('Please inform the experimenter that you are done.', 0, 120);
        KbStrokeWait;
    end % end block loop

catch
    ple;
end

Priority(0);
ListenChar;
ShowCursor;
fprintf('\n# of open windows = %d\n', numel(Screen('Windows')));
Screen('CloseAll');
PsychPortAudio('Close');
clear all;



function paintFrame(coordinates, nObjects, diskColors, window)

global screenRect displayRect objectRect
global colWhite colBlack colMidGray colDarkGray colYellow colRed


screen(window, 'FillRect', colMidGray);
for object = 1:nObjects
    placeRect = CenterRectOnPoint(objectRect, coordinates(object, 1), coordinates(object, 2));
    screen(window, 'FillOval', diskColors(object, :), placeRect);
end


function varargout = DialogBox (title, varargin)

n = (nargin - 1);
if nargout ~= n / 3
    error('input and output arguments must match');
end
prompt = varargin(1:3:n);
defaults = varargin(2:3:n);
toNum = varargin(3:3:n);
param = inputdlg(prompt, title, 1, defaults);
if isempty(param)
    error('Dialog box cancelled');
end
varargout = cell(1, nargout);
for i = 1:length(param)
    p = param{i};
    if toNum{i}
        n = [];
        if ~exist(p)
            n = str2num(p);
            if ~isempty(n)
                varargout{i} = n;
            end
        end
        if isempty(n)
            error('parameter ''%s'' value ''%s'' could not be converted to numeric as requested', ...
                  prompt{i}, p);
        end
    else
        varargout{i} = p;
    end
end


function [newX, newY] = CenterText (message, xoffset, yoffset, color, window)
% print a text string centered on the screen
% syntax [newX, newY] = CenterText (message, [xoffset], [yoffset], [color], [window])
% if you want the text offset from center, use xoffset and yoffset
% if window is not specified, prints to winMain, which must be a global in the calling function
% 2/23/2000 accepts color option
% 4/23/2002 can now print to offscreen windows

global screenX
global screenY
global winMain

switch nargin
  case 1
    xoffset=0;
    yoffset=0;
    color = [];
    window = winMain;
  case 2
    yoffset=0;
    color = [];
    window = winMain;
  case 3
    color = [];
    window = winMain;
  case 4
    window = winMain;
end

windowRect = screen(window, 'Rect');
width = screen(window, 'TextWidth', message);
[newX, newY] = screen(window, 'DrawText', message, ((windowRect(3)/2)-(width/2))+xoffset, (windowRect(4)/2)+yoffset, color);


function [newX, newY] = CenterCellText(window, messages, spacing, color)

% prints a cell array of text strings centered on the screen
% syntax [newX, newY] = CenterCellText(window, messages, [spacing], [color])
% messages is a cell array of lines of text
% spacing governs the distance between lines of text, in pixels
% default spacing is 20 pixels
% added color option 1/17/2003; only does one color for the whole thing
% fixed number of options bug 01/24/2003

defaultSpacing = 20;
switch nargin
  case 0
    error('CenterCellText requires at least two arguments!');
  case 1
    error('CenterCellText requires at least two arguments!');
  case 2
    spacing = defaultSpacing;
    color = [];
  case 3
    color = [];
end

lines = length (messages);

% find the middle line
middleLine = round(lines/2);

yOffset = spacing*((1:lines)-middleLine);
for y = 1:lines
    [newX, newY] = CenterText(messages{y}, 0, yOffset(y), color, window);
end


function trajectories = makeTrajectories(nObjects, nFrames, objectSize)

global screenRect displayRect

% generates MVT trajectories
% given the number of objects and frames, returns positions for each object for each frame
% code adapted from Jen Dimase's motPictMem, Justin Junge
% started 9/10/2004
% current 9/14/2004

%  Creates target Locations.

% coordinate system
centx = round(screenRect(3)/2);
centy = round(screenRect(4)/2);
cellSize = round(displayRect(3:4)/7); % size of initial position grid cell

[fieldRect, xOffset, yOffset] = CenterRect(displayRect, screenRect);

x = 0:6;
xloc = xOffset + cellSize(1) * x;
yloc = yOffset + cellSize(2) * x;

[gridy, gridx] = meshgrid(yloc, xloc);

shufflegrid = randperm(49);
shufflegrid = shufflegrid(1:nObjects);
trajectories(:, 1, 1) = gridx(shufflegrid(:));
trajectories(:, 2, 1) = gridy(shufflegrid(:));

% motion parameters
repulsionPower = 10000; % not sure where the value comes from
inertia = 1;
pathchange = 1.5;		                           %	Determines Degree of Change on Motion Paths
forcefieldDistance = 1.5 * objectSize;	% 1.5 = Item Repulsion only occurs within a field around each shape 25% the size of the shape. 

initMotion = [-4:4] * pathchange;
frameMotion = [-2:2] * inertia;
rXmove = initMotion(randi(9, [1, nObjects]));
rYmove = initMotion(randi(9, [1, nObjects]));

for frame = 2:nFrames
    
    aa = frame-1;
    
    % Repulsion 
    repel = ones(nObjects, 2) * 0.00001;
    
    for object=1:nObjects;
        thisLocation = repmat(trajectories(object, :, aa), nObjects, 1); % creates nObjects X 2 array of object object's coordinates
        distances  = thisLocation - trajectories(:, :, aa);
        absoluteDistances = abs(distances);
        % 		i = find((absoluteDistance(1) < forcefieldDistance) & (absoluteDistance(2) < forcefieldDistance));
        
        for qq=1:nObjects;
            if qq ~= object
                distance = trajectories(object, :, aa) - trajectories(qq, :, aa);
                absoluteDistance = abs(distance);
                if (absoluteDistance(1) < forcefieldDistance) & (absoluteDistance(2) < forcefieldDistance)
                    if distance(1) ~= 0 & distance(2) ~= 0
                        addrepel = 1./((distance.^2) .* sign(distance));
                        repel(object, :) = repel(object, :) + repulsionPower * addrepel;
                    end
                end
            end
        end
    end

    rXchange = frameMotion(randi(5, [1, nObjects]));
    rYchange = frameMotion(randi(5, [1, nObjects]));
    newrm = [-1 1];

    % X Trajectories
    
    for object = 1:nObjects
        
        rXmove(object) = rXmove(object) + rXchange(object) + repel(object, 1);										% Makes change to X motion
        
        % floor and ceiling
        
        if rXmove(object)==0;
            rXmove(object) = newrm(randi(2));
        end
	
        if rXmove(object) > 4*pathchange
            rXmove(object) = 4*pathchange;
        end
        if rXmove(object) < -4*pathchange
            rXmove(object) = -4*pathchange;
        end
	
        trajectories(object, 1, frame) = (trajectories(object, 1, aa) + rXmove(object));
        
        if trajectories(object, 1, frame) >= (fieldRect(3) - objectSize); 		% Bounces off right
            trajectories(object, 1, frame) = (fieldRect(3) - objectSize);
            rXmove(object) = -(4*rXmove(object));	
        end			
        if  trajectories(object, 1, frame) <= fieldRect(1);					% Bounces off left
            trajectories(object, 1, frame) = fieldRect(1);
            rXmove(object) = -(4*rXmove(object));											
        end	
    end

    % Y Trajectories
    for object = 1:nObjects
        
        rYmove(object) = rYmove(object) + rYchange(object) + repel(object, 2);										% Makes change to Y motion
        
        if rYmove(object)==0;
            rYmove(object) = newrm(randi(2));
        end
        
        if rYmove(object) > 4*pathchange
            rYmove(object) = 4*pathchange;
        end
        if rYmove(object) < -4*pathchange
            rYmove(object) = -4*pathchange;
        end
	
        trajectories(object, 2, frame) = (trajectories(object, 2, aa) + rYmove(object));							
        if trajectories(object, 2, frame) >= (fieldRect(4) - objectSize);													% Bounces off Bottom
            trajectories(object, 2, frame) = (fieldRect(4) - objectSize);
            rYmove(object) = -(4*rYmove(object));	
        end
	
        if 	trajectories(object, 2, frame) <= fieldRect(2);																% Bounces off Top
            trajectories(object, 2, frame) = fieldRect(2);
            rYmove(object) = -(4*rYmove(object));									
        end
    end
    d(frame, :) = sqrt(rXmove.^2 + rYmove.^2);
    
end
% mean(d)
