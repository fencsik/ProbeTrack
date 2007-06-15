function ProbeTrack

% get RT response to MVT task with gap
% how long does it take to recover targets?
% vary tracking load
% started 4/26/2005
% current 4/26/2005
% skyler added: 	9/28 - made cursor disappear, and added "configuring trial" screen inbetween trials. 
%				            also adjusted size of font for testing room screens.
%                             11/11 - adjusted SOA defaults for ProbeTrack2 requirements.
%					    also changed program name and file output to reflect 2nd experiment.
%				2/4    - adjusted SOA defaults for ProbeTrack3 requirements.
%					    also changed program name and file output to reflect 3rd experiment.	

% $LastChangedDate$

% declare global variables
global MainWindow display
global screenRect displayRect objectRect
global white black midGray darkgray yellow red

experiment = 'ProbeTrack6';

% define rects
screenRect = [0 0 1024 768];
displayRect = [0 0 500 500];
fieldRect = CenterRect(displayRect, screenRect);
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

keys = [KbName('''')  KbName('a')]; % response key assignments

% get user input
[identifier, practiceTrials, experimentalTrials, nObjects, nTargets, ...
 cueDuration, gapDuration, SOAlist, gapOnsetRange, postProbeDuration] = ...
   DialogBox('Experiment Parameters', ...
             'Subject identifier'       , 'xxx', ...
             'practice trials'          , '16', ...
             'experimental trials'      , '120', ...
             'number of objects'        , '8', ...
             'number of targets'        , '4', ...
             'cue duration (frames)'    , '60', ...
             'gap duration'             , '23', ...
             'SOAs'                     , '0 3 6 12', ...
             'gap onset range (frames)' , '60 180', ...
             'post-probe duration'      , '60');

% define colors
midGray = [128 128 128];
darkGray = [64 64 64];
yellow = [240 240 0];
red = [250 0 0];

% setup video
MainWindow = screen(0, 'OpenWindow', [0 0 0], screenRect);
display = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);
cueFrame = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);

% define colors
black = BlackIndex(MainWindow);
white = WhiteIndex(MainWindow);
% get refresh rate
hz = screen(MainWindow, 'FrameRate');
hz = round(hz);

% now define color sets for each phase of the trial
trackingColors= repmat(darkGray, nObjects, 1);
cueingColors = trackingColors;
cueingColors(1:nTargets, :) = repmat(yellow, nTargets, 1);
gapColors = repmat(midGray, nObjects, 1);

% miscellaneous setup
dataFileName = [experiment, 'Data-', identifier, '.txt'];
rand('state', 100 * sum(clock));

% setup data file
% format for data output
headerFormatString	=	'%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s \n';
dataFormatString = 		'%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d \n';
if exist(dataFileName, 'file') == 0
	dataFile = fopen(dataFileName, 'a');
	fprintf(dataFile, headerFormatString, 'sub', 'hz',  'nobjects', 'ntargets', 'cuedur', 'gapdur', 'soa', 'min_gapOnset', 'max_gapOnset', 'postProbeDuration', 'block', 'trial', 'gapOnset', 'probeType', 'error', 'badkey', 'rt');
	fclose(dataFile);
end

% present instructions
hidecursor

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

screen(MainWindow, 'TextFont', 'Palatino');
screen(MainWindow, 'TextSize', 24);


screen(MainWindow, 'FillRect', midGray);
CenterCellText(MainWindow, Instructions, 30);
CenterText('Press any key to continue', 0, 300);
FlushEvents('keyDown');
GetChar;



screen(MainWindow, 'TextFont', 'Skia');
screen(MainWindow, 'TextSize', 26);

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
        
	screen(MainWindow, 'FillRect', midGray);
	CenterText(['Press any key to start ', num2str(nTrials), ' ', message, ' trials']);
	FlushEvents('keyDown');
	GetChar;
	
	% trial routine
	screen(MainWindow, 'FillRect', midGray);
	CenterText('configuring trial...', 0, 0);
	
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
		probeColors(probeItem, :) = red; % set colors for probe frame
		
		% now put up initial display, wait for keypress to initiate trial, then cue targets
		screen(cueFrame, 'FillRect', midGray);
		screen(display, 'FillRect', midGray);
		paintFrame(trajectories(:, :, 1), nObjects, trackingColors, display);
		paintFrame(trajectories(:, :, 1), nObjects, cueingColors, cueFrame);
		
		screen(MainWindow, 'WaitBlanking');
		screen('CopyWindow', display, MainWindow);
		CenterText(['Press a key to start trial ', num2str(trial)], 0, 300);
		FlushEvents('keyDown');
		GetChar;
		screen(MainWindow, 'WaitBlanking');
		screen('CopyWindow', cueFrame, MainWindow);
		screen(MainWindow, 'WaitBlanking', cueDuration);
		
		postProbeFrames = 0;
		response = -1;
		% motion sequence
		for frame =1:gapOnsetTime(trial)
			% pre-gap interval
			paintFrame(trajectories(:, :, frame), nObjects, trackingColors, display);
			screen(MainWindow, 'WaitBlanking');
			screen('CopyWindow', display, MainWindow);
		end
		for gLoop = 1:gapDuration
			% gap interval
			frame = frame + 1;
			paintFrame(trajectories(:, :, frame), nObjects, gapColors, display);
			screen(MainWindow, 'WaitBlanking');
			screen('CopyWindow', display, MainWindow);
		end
		for sLoop = 1:SOA(trial)
			% SOA interval
			frame = frame + 1;
			paintFrame(trajectories(:, :, frame), nObjects, trackingColors, display);
			screen(MainWindow, 'WaitBlanking');
			screen('CopyWindow', display, MainWindow);
		end			
		probeOnsetTime = GetSecs;
		% this next bit of code looks for a response in between refreshes
                responseTime = 0;
		while (postProbeFrames < postProbeDuration)&(response == -1)
			postProbeFrames = postProbeFrames + 1;
			frame = frame + 1;
			paintFrame(trajectories(:, :, frame), nObjects, probeColors, display);
			[startRefresh, startTime] = screen(MainWindow, 'PeekBlanking');
			currentRefresh = startRefresh;
			while currentRefresh == startRefresh
				[currentRefresh, currentTime] = screen(MainWindow, 'PeekBlanking');
				[keyIsDown, KbTime, keyCode] = KbCheck;
				if keyIsDown
					response = find(keyCode);
					response = response(1);
					responseTime = KbTime;
				end
			end
			screen('CopyWindow', display, MainWindow);
		end
		RT = round((responseTime - probeOnsetTime) * 1000); % RT in ms
		
		% now classify response
		
		responseKey = find(keys == response);
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
	
		screen(MainWindow, 'FillRect', midGray);
		CenterText([feedback, ' - RT = ', num2str(RT), ' ms']);
		WaitSecs(1);
		
		% save data
		dataFile = fopen(dataFileName, 'a');
		fprintf(dataFile, dataFormatString, identifier, hz,  nObjects, nTargets, cueDuration, gapDuration, SOA(trial), gapOnsetRange(1), gapOnsetRange(2), postProbeDuration, block, trial, gapOnsetTime(trial), probeType(trial), error, wrongKeyFlag, RT);
		fclose(dataFile);
		
	end % end trial loop
end % end block loop

% now clean up and go home
screen(MainWindow, 'FillRect', midGray);
CenterText('Thank you for participating!');
CenterText('Please inform the experimenter that you are done.', 0, 120);
FlushEvents('keyDown');
GetChar;
clear screen;

function paintFrame(coordinates, nObjects, diskColors, window)

global screenRect displayRect objectRect
global white black midGray darkgray yellow red


screen(window, 'FillRect', midGray);
for object = 1:nObjects
	placeRect = CenterRectOnPoint(objectRect, coordinates(object, 1), coordinates(object, 2));
	screen(window, 'FillOval', diskColors(object, :), placeRect);
end


function varargout = DialogBox (title, varargin)

n = (nargin - 1);
if nargout ~= n / 2
   error('input and output arguments must match');
end
prompt = varargin(1:2:n);
defaults = varargin(2:2:n);
param = inputdlg(prompt, title, 1, defaults);
if isempty(param)
   varargout = {};
   return;
end
varargout = cell(1, nargout);
for i = 1:length(param)
   p = param{i};
   n = [];
   if ~exist(p), n = str2num(p); end
   if isempty(n)
      varargout{i} = p;
   else
      varargout{i} = n;
   end
end


function [newX, newY] = CenterText (message, xoffset, yoffset, color, window)
% print a text string centered on the screen
% syntax [newX, newY] = CenterText (message, [xoffset], [yoffset], [color], [window])
% if you want the text offset from center, use xoffset and yoffset
% if window is not specified, prints to MainWindow, which must be a global in the calling function
% 2/23/2000 accepts color option
% 4/23/2002 can now print to offscreen windows

global screenX
global screenY
global MainWindow

switch nargin
case 1
	xoffset=0;
	yoffset=0;
	color = [];
	window = MainWindow;
case 2
	yoffset=0;
	color = [];
	window = MainWindow;
case 3
	color = [];
	window = MainWindow;
case 4
	window = MainWindow;
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
