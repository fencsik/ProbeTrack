function mvt()

% mvt.m: Multi-disk tracking for training/practice.
% Authors: Todd Horowitz, David Fencsik, Randy Birnkrant
% $Id: train.m,v 1.4 2003/12/19 15:40:47 fencsik Exp $

% just basic tracking for practice
% syntax asynchtrack1()
% uses tGenerator6a; generate a set of practice trials and a set of experimental trials
% based on asynchtrack1 of 7/09/2003
% created 7/09/2003
% version of 7/09/2003

% this version uses only one paths file
% fix data output so "pathFile" is output 7/11/03 rsb
% version of 9/23/2003 - unselected correct ball feedback
% 10/*/2003 - added support for stGenerator for path files generated for shifttrack

global screenX                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
global screenY
global MainWindow
global nDirections

dataFileName = 'mvtData';

% min frame duration in ms: if average frame duration is greater than this
% value, then panic and exit. The MATLAB interpreter is sometimes slow and
% can't draw fast enough, and this accomodates that.
minFrameDuration = 20;

prompt = {'subject''s initials', 'disks to track','paths file?'};
defaults = {'xxx', '5', 'train1'};
answer = inputdlg(prompt, 'Experimental Setup Information', 1, defaults);

% now decode answer
[subject, tracknumber, pathsFile] = deal(answer{:});
tracknumber = str2num(tracknumber);

% derive initial 3 letters of subject name 
[junk nameSize] = size(subject);
if nameSize < 3
	sInitial = subject;
	sInitial((nameSize+1):3) = 'x';
else
	sInitial = subject(1:3);
end


% data file setup:
dataFile = fopen(dataFileName, 'a');
fprintf(dataFile, 'sub,pathsFile,trial,speed,disks,tracknumber,frameDur,error,nerrors\n');
fclose(dataFile);
formatString = '%s,%s,%d,%d,%d,%d,%f,%d,%d\n';
% subject, pathsFile, trial, movementRate, nDisks, tracknumber, averageFrameDuration, error(trial), nErrors(trial)

% video setup

imageX=35;
imageY=35;
imageRect = [0 0 imageX imageY];
coreRect = imageRect - [0 0 4 4];
ballRect = CenterRect(coreRect, imageRect);
MainWindow = screen(0, 'OpenWindow', [], [], 8);
screenRect = screen(0, 'Rect');
screenX = screenRect(3);
screenY = screenRect(4);
hz = round(screen(MainWindow, 'FrameRate'))


white = WhiteIndex(MainWindow);
black = BlackIndex(MainWindow);
gray = round((white+black)/2);
if round(gray)==white
	gray=black;
end
darkGray = (gray+black)/2;
lightGray = (gray+white)/2;
% now set colors
red = 1;
blue = 2;
green = 3;
yellow = 4;
clut = screen(MainWindow, 'GetClut');
clut (red + 1, :) = [255 0 0];
clut (blue + 1, :) = [0 0 255];
clut (green + 1, :) = [0 255 0];
clut (yellow +1, :) = [255 255 0];
LoadClut(MainWindow, clut);


screen(MainWindow, 'FillRect', gray);
screen(MainWindow, 'TextFont', 'Times');
screen(MainWindow, 'TextSize', 24);
hidecursor;
% framesPerMovieFrame = 2;

% load functions into memory
% these functions take time to load into memory, but will remain in memory until cleared

screen('Screens'); %this function opens or cloeses windows on or off screen
centerrect(imageRect, screenRect);% centers first rect in second rect
KbCheck; % checks whether a key is pressed down
GetSecs; % returns the number of seconds since the computer started

% set the state of the random number generator to some random value (based on the clock)
tau = clock;
rand('state',sum(100*tau));
state=rand('state');
seedFileName = ['MVTseed-', sInitial];
save (seedFileName, 'tau');

% open other windows

% off screen stimulus window
stimulus(1) = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);
stimulus(2) = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);
screen(stimulus(1), 'FillRect', gray);
screen(stimulus(2), 'FillRect', gray);

% big eraser
screenBlank=screen(MainWindow, 'OpenOffscreenWindow',  gray, screenRect); 

%make some beeps
[beep, samplingRate] = makeBeep(650,.1);
[errbeep, samplingRate] = MakeBeep(850,.11);
% load paths file 
load(pathsFile);
%nTrials = 3;
% predictedMovieFrameDuration = 40; % expected frame duration; if this is wrong, then the trial durationwill be off, as well as the speed
totalMovieFrames = round(1000*(trialDuration/predictedMovieFrameDuration));

% define the stimulus presentation loop

loop = {
'		for frame = 1:finalFrame;'
'			thisFrame = mod(frame - 1, 2) + 1;'
'			lastFrame = mod(frame, 2) + 1;'
'			time1 = GetSecs;'
'			for n = 1:nDisks;'
'				if frame > 1;'
'					screen(''CopyWindow'', eraserPointer, stimulus(lastFrame), imageRect, lastPlaceRect{n}, ''transparent'');'
'				end;'
'				placeRect{n} = [trajectory(n, 1, frame) - imageX trajectory(n, 2, frame) - imageY trajectory(n, 1, frame) trajectory(n, 2, frame)];'
'				pointer = ballPointer;'
'				screen(''CopyWindow'', pointer, stimulus(thisFrame), imageRect, placeRect{n}, ''transparent'');'
'			end;'
'			screen(MainWindow, ''WaitBlanking'', 1);'
'			screen(''CopyWindow'', stimulus(thisFrame), MainWindow);'
'			frameDuration(frame) = GetSecs - time1;'
'			lastPlaceRect = placeRect;'
'		end;'
};
pointerNames = {'ballPointer', 'currentSelectionBall', 'unselectedBall', 'selectedCorrectBall', 'unselectedCorrectBall', 'selectedErrorBall'};
diskBackgroundColors = {'[]', 'lightGray', 'gray', 'green', 'yellow', 'red'};
diskForegroundColors = {'darkGray', 'darkGray', 'darkGray', 'darkGray', 'darkGray', 'darkGray'};
for p = 1:6
	eval([pointerNames{p}, ' = screen(MainWindow, ''OpenOffScreenWindow'', ', diskBackgroundColors{p}, ', imageRect);']);
	eval(['screen(', pointerNames{p}, ', ''FillOval'', black, imageRect);']);
	eval(['screen(', pointerNames{p}, ', ''FillOval'',', diskForegroundColors{p}, ', ballRect);']);
end
eraserPointer = screen(MainWindow, 'OpenOffScreenWindow', [], imageRect);
screen(eraserPointer, 'FillOval', gray, imageRect);


% instructions
nDiskString = num2str(nDisks);
tracknumberString = num2str(tracknumber);

taskInstructionString = {
'In this experiment, you are asked to track a number of disks moving around the screen';
['At the beginning of each trial, you will see ', nDiskString, ' dark gray disks on a medium gray background'];
[tracknumberString, ' of these disks will blink on and off. These are your targets.'];
'After the targets stop blinking, all the disks will start to move around the screen.';
['Your task is to keep track of the ', tracknumberString, ' targets as they move.'];
};


responseInstructionString = {
'When the disks stop moving, the arrow cursor will appear.';
'Use the mouse to click on all of the targets.';
'The disk you have selected will be highlighted in green if you are correct';
'and you will hear a low tone.';
'If you click on a non-target, it will be highlighted in red';
'and you will hear a high tone.';
['After you have clicked ', tracknumberString, ' times, you may press a key to go on to the next trial.'];
};

screen('CopyWindow', screenBlank, MainWindow);
CenterCellText(MainWindow, taskInstructionString, 50);
CenterText('press any key to continue', 0, 250);
FlushEvents('keyDown');
GetChar;
screen('CopyWindow', screenBlank, MainWindow);
CenterCellText(MainWindow, responseInstructionString, 50);
CenterText('press any key to continue', 0, 250);
FlushEvents('keyDown');
GetChar;
screen('CopyWindow', screenBlank, MainWindow);


screen('CopyWindow', screenBlank, MainWindow);
[newX newY] = CenterText(['Press any key to begin ', num2str(nTrials), ' trials']);
FlushEvents('keyDown');
GetChar;

error = zeros(nTrials,1);
nErrors = error;

slowFlag = 0;

for trial = 1:nTrials
	
	trialString = num2str(trial);
	
% 	blankWindow(1) = round(2000/predictedMovieFrameDuration); % blank comes at least 2 seconds after start of tracking
% 	blankWindow(2) = totalMovieFrames - round(1000/predictedMovieFrameDuration); % blank comes no later than 1 seconds before end of tracking 

	finalFrame = movieFrames(trial); % finalFrame is leftover from dougtrack4
	
	% get coordinates
	trajectory = paths{trial};
	startCoordinates = starts{trial};
	
	%present the stimuli
	hidecursor;
	
	% blank both stimulus screens
	screen('CopyWindow', screenBlank, stimulus(1)); 
	screen('CopyWindow', screenBlank, stimulus(2)); 
	screen('CopyWindow', screenBlank, MainWindow);
	[newX newY] = CenterText(['Press any key to begin trial ', trialString]);
	FlushEvents('keyDown');
	GetChar;
	for a = 1:nDisks
		screen('CopyWindow', ballPointer, stimulus(2), [], [startCoordinates(a,1) - imageX startCoordinates(a,2) - imageY startCoordinates(a,1) startCoordinates(a,2)], 'transparent');
		if a > tracknumber
			screen('CopyWindow', ballPointer, stimulus(1), [], [startCoordinates(a,1) - imageX startCoordinates(a,2) - imageY startCoordinates(a,1) startCoordinates(a,2)], 'transparent');
		end
	end

	%flash the tracked balls 4 times to cue them
	for flash = 1:9
		screen('CopyWindow', stimulus(mod(flash,2)+1), MainWindow);
		WaitSecs(.5);
	end

	%clear the stimulus window
	% 		screen('CopyWindow', screenBlank, MainWindow);
	screen('CopyWindow', screenBlank, stimulus(1));
	screen('CopyWindow', screenBlank, stimulus(2));
	FlushEvents('keydown');
	
	% now present stimuli
	
	initTime = getsecs; %check the start time time
	time1 = initTime;
	frameDuration = zeros(totalMovieFrames,1);
	
	priorityLevel=MaxPriority(MainWindow,'WaitBlanking');
	rush(loop,priorityLevel);
	trialDuration = GetSecs - initTime
	averageFrameDuration = mean(frameDuration)*1000
	% 		max(frameDuration)*1000
	% 		min(frameDuration)*1000
	
    % if framerate is too slow, then exit with a warning:
    if averageFrameDuration > minFrameDuration
        slowFlag = averageFrameDuration;
        feedbackString = {'COMPUTER ERROR:  SLOW FRAME DURATION';
                          ['Average frame duration = ' num2str(slowFlag)];
                          '';
                          'The block needs to be restarted.';
                          'Please inform the experimenter immediately.';
                          '';
                          'Experimenter: Press any button to exit,';
                          'then restart the block.'
                      };
        screen('CopyWindow', screenBlank, MainWindow);
        CenterCellText(MainWindow, feedbackString, 30);
        FlushEvents('keyDown');
        GetChar;
        break;
    end;

	% chill out
	% screen(MainWindow, 'WaitBlanking', 10);
	%at end of trial, check the response with a mouseclick
	testFrame = frame;
	
	FlushEvents(['mouseUp'],['mouseDown']);
	ShowCursor([0]);
	% 		screen(MainWindow, 'FillRect', darkGray);
	locationError = ones(1, tracknumber);%everything starts as an error
	
	% compute center of each stimulus
	nClicks = 0;
	diskVector = zeros(1, nDisks);
	while nClicks < tracknumber
		button = 0;
		% 			nClicks = nClicks +1;
		
		while button == 0
			% redraw stimulus
			[x, y, button] = getmouse;
			for d = 1:nDisks
				placeRect{d} = [trajectory(d, 1, testFrame) - imageX trajectory(d, 2, testFrame) - imageY trajectory(d, 1, testFrame) trajectory(d, 2, testFrame)];
				switch diskVector(d)
				case 1
					screen('CopyWindow', selectedCorrectBall, MainWindow, imageRect, placeRect{d});
				case 2
					screen('CopyWindow', selectedErrorBall, MainWindow, imageRect, placeRect{d});
				otherwise
					if IsInRect(x, y, placeRect{d})
						screen('CopyWindow', currentSelectionBall, MainWindow, imageRect, placeRect{d});
					else
						screen('CopyWindow', unselectedBall, MainWindow, imageRect, placeRect{d});
					end
				end
			end
			[x, y, button] = getmouse;
			if button == 1 %if the mouse button is pressed
				while button == 1 %wait until it is released
					[x, y, button] = getmouse; %get the x,y coordinates of the location at which the mouse button was released
				end
				button = 1; %reset the button to 1 so that you can break out of the loop
			end
		end
	
	
		% is mouse within this rect?
		clickedInARect = 0;
		for d = 1:nDisks
			% 				testFrame
			placeRect{d} = [trajectory(d, 1, testFrame) - imageX trajectory(d, 2, testFrame) - imageY trajectory(d, 1, testFrame) trajectory(d, 2, testFrame)];
			if IsInRect(x, y, placeRect{d})
				if diskVector(d) == 0
					clickedInARect = 1;
					if d <= tracknumber
						screen('CopyWindow', selectedCorrectBall, MainWindow, imageRect, placeRect{d});
						snd('play', beep);
						locationError(d) = 0;
						diskVector(d) = 1;
					else
						screen('CopyWindow', selectedErrorBall, MainWindow, imageRect, placeRect{d});
						snd('play', errbeep);
						diskVector(d) = 2;
					end
				end %if diskVector(d) == 0
			end %if IsInRect(x, y, placeRect{d})
		end %for d = 1:nDisks
		if clickedInARect == 1
			nClicks = nClicks + 1;
		end
	
	
	end % while nClicks...

		% if there were any errors, then flash the unselected correct disks:
		if any(diskVector == 2)
			for d = 1:nDisks
				if d <= tracknumber
					if diskVector(d) == 0
						screen('CopyWindow', unselectedBall, stimulus(2), [], placeRect{d}, 'transparent');
						screen('CopyWindow', unselectedCorrectBall, stimulus(1), [], placeRect{d}, 'transparent');
					else
						screen('CopyWindow', selectedCorrectBall, stimulus(2), [], placeRect{d}, 'transparent');
						screen('CopyWindow', selectedCorrectBall, stimulus(1), [], placeRect{d}, 'transparent');
					end
				elseif d > tracknumber
					if diskVector(d) == 0
						screen('CopyWindow', ballPointer, stimulus(2), [], placeRect{d}, 'transparent');
						screen('CopyWindow', ballPointer, stimulus(1), [], placeRect{d}, 'transparent');
					else	
						screen('CopyWindow', selectedErrorBall, stimulus(2), [], placeRect{d}, 'transparent');
						screen('CopyWindow', selectedErrorBall, stimulus(1), [], placeRect{d}, 'transparent');
					end
				end
			end
			%flash the unselected correct balls 3 times
			for flash = 1:6
				screen('CopyWindow', stimulus(mod(flash,2)+1), MainWindow);
				WaitSecs(.333);
			end
		end
		
% 	for d = 1:tracknumber
% 		if diskVector(d) == 0
% 			for x=1:3
% 				screen('CopyWindow', unselectedBall, MainWindow, imageRect, placeRect{d});
% 				WaitSecs(.2);	
% 				screen('CopyWindow', unselectedCorrectBall, MainWindow, imageRect, placeRect{d});					WaitSecs(.2);
% 			end
% 		end
% 	end
	error(trial) = max(locationError);
	nErrors(trial) = sum(locationError);
	% save data
	dataFile = fopen(dataFileName, 'a');
	count = fprintf(dataFile, formatString, subject, pathsFile, trial, movementRate, nDisks, tracknumber, averageFrameDuration, error(trial), nErrors(trial));%practicePathsFile
	fclose('all');
	[newX newY] = CenterText('Press any key to continue', 0, 0, black);
	FlushEvents('keyDown');
	GetChar;
end % trial loop

if slowFlag == 0
    fprintf(1, 'Mean Accuracy = %1.2f\n', 1 - mean(error));
    fprintf(1, 'Mean # Disks Tracked = %1.2f\n', tracknumber - mean(nErrors));
end;

clear screen;
