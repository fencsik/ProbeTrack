function probeTrack3b()

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
               

% to do:
% 3- , data saving, 

% declare global variables
global MainWindow display
global screenRect displayRect objectRect
global white black midGray darkgray yellow red

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
prompt = {'Subject identifier', 'practice trials', 'experimental trials', 'number of objects', 'number of targets', 'cue duration (frames)', 'gap duration', 'SOAs', 'gap onset range (frames)', 'post-probe duration'};
defaults = {'dummy', '5', '50', '8', '4', '60', '23', '0 6 24 96', '60 180', '60'};

answer = inputdlg(prompt, 'experiment setup info', 1, defaults);
% now decode answer
[identifier, practiceTrials, experimentalTrials, nObjects, nTargets, cueDuration, gapDuration, SOAlist, gapOnsetRange, postProbeDuration] = deal(answer{:});
practiceTrials = str2num(practiceTrials);
experimentalTrials = str2num(experimentalTrials);
nObjects = str2num(nObjects);
nTargets = str2num(nTargets);
cueDuration = str2num(cueDuration);
gapDuration = str2num(gapDuration);
SOAlist = str2num(SOAlist);
gapOnsetRange = str2num(gapOnsetRange);
postProbeDuration = str2num(postProbeDuration);

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


% setup data file
% format for data output
headerFormatString	=	'%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s, %s,%s,%s,%s \n';
dataFormatString = 		'%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d, %d,%d,%d \n';
if exist('probeTrack3bData', 'file') == 0
	dataFile = fopen('probeTrack3BData', 'a');
	fprintf(dataFile, headerFormatString, 'identifier', 'hz',  'nObjects', 'nTargets', 'cueDuration', 'gapDuration', 'SOA', 'min_gapOnset', 'max_gapOnset', 'postProbeDuration', 'block', 'trial', 'gapOnset', 'probeType', 'error', 'wrongKeyFlag', 'RT');
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
	gapOnsetTime = randi(gapOnsetRange(2) - gapOnsetRange(1), [nTrials, 1]) + gapOnsetRange(1); % sets random gap duration for all trials
	SOA = SOAlist(randi(length(SOAlist), [nTrials, 1])); % randomly selects SOA for each trial
	probeType = randi(2, [nTrials, 1]); % randomly selects whether to probe target or distractor on each trial
	
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
		dataFile = fopen('probeTrack3bData', 'a');
		fprintf(dataFile, dataFormatString, identifier, hz,  nObjects, nTargets, cueDuration, gapDuration, SOA(trial), gapOnsetRange(1), gapOnsetRange(2), postProbeDuration, block, trial, gapOnsetTime(trial), probeType(trial), error, wrongKeyFlag, RT);
		fclose(dataFile);
		
	end % end trial loop
end % end block loop

% now clean up and go home
screen(MainWindow, 'FillRect', midGray);
CenterText('Thank you for participating!');
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
