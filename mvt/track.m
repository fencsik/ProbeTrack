function shifttrack()

% $Id: track.m,v 1.7 2004/02/06 20:19:45 fencsik Exp $

% Can observers tolerate a longer gap duration if all items disappear at once?
% constant blank duration method rather than staircase
% syntax asynchtrack3()
% uses tGenerator6a; generate a set of practice trials and a set of experimental trials
% based on asynchtrack1 of 7/14/03
% created 9/23/2003
% version of 9/23/2003
% version of 9/23/2003 - unselected correct ball feedback, removed staircase, data output just accuracy (RSB)
% version of 10/3/2003 - modified to passively run pathfile generated by tgenerator8a.m, added trial level feedback (DEF)
% version of 10/15/2003 - made into ShiftTrack2, saved more data (DEF)
% 11/10/2003 - added check to ensure frame duration is approx. 13 ms. If it
%              seems to be over, then exit cleanly and tell RA to restart.

   
global screenX
global screenY
global MainWindow
global nDirections

ExperimentName = 'SwitchTrack1';
minimumAccuracy = '60%';
% min frame duration in ms: if average frame duration is greater than this
% value, then panic and exit. The MATLAB interpreter is sometimes slow and
% can't draw fast enough, and this accomodates that.
minFrameDuration = 20;

tracer = 0;
logging = 0;
if logging;
   LogFileName = [ExperimentName '.log'];
   logFile = fopen(LogFileName,'a');
   fprintf(logFile, '\nStarted on %s\n', datestr(now));
end;

dataFileName = [ExperimentName '_Data.txt'];

prompt = {'Subject''s initials',
          'Number of disks to track (unused)',
          'Practice paths file',
          'Paths file',
          'Move type (1 = moving, 2 = static)',
          'Response type (1 = full, 2 = 2AFC)'
          'Randomize trials (0 = no, 1 = yes)',
          };
defaults = {'xxx', '0', 'test1', 'exp_1', '1', '2', '0'};
answer = inputdlg(prompt, 'Experimental Setup Information', 1, defaults);
% check for cancel button-press
if size(answer) < 1
   'Experiment Cancelled'
   return
end;

% now decode answer
[subject, nTargets, practicePathsFile, experimentalPathsFile, moveType, responseType, randomizeTrials] = deal(answer{:});
nTargets = str2num(nTargets);
moveType = 1; %str2num(moveType);
randomizeTrials = 0; %str2num(randomizeTrials);
responseType = str2num(responseType);

% derive initial 3 letters of subject name 
[junk nameSize] = size(subject);
if nameSize < 3
	sInitial = subject;
	sInitial((nameSize+1):3) = 'x';
else
	sInitial = subject(1:3);
end


% other major variables
dataFile = fopen(dataFileName, 'a');
fprintf(dataFile,'sub,time,pracfile,pathfile,blocktype,movetype,trialdur,blankdur,trial,line,speed,shift,ndisks,ntargets,framedur,error,nerrors\n');
fclose(dataFile);


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
hz = round(screen(MainWindow, 'FrameRate'));

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
clut (yellow + 1, :) = [255 255 0];
LoadClut(MainWindow, clut/255*1023);


screen(MainWindow, 'FillRect', gray);
screen(MainWindow, 'TextFont', 'Times');
screen(MainWindow, 'TextSize', 24);
hidecursor;
% framesPerMovieFrame = 2;

% load functions into memory
% these functions take time to load into memory, but will remain in memory until cleared

screen('Screens'); % this function opens or cloeses windows on or off screen
centerrect(imageRect, screenRect);% centers first rect in second rect
KbCheck; % checks whether a key is pressed down
GetSecs; % returns the number of seconds since the computer started

% set the state of the random number generator to some random value (based on the clock)
tau = clock;
rand('state',sum(100*tau));
state=rand('state');
seedFileName = ['seed-', sInitial, 'C'];
save (seedFileName, 'tau');

% open other windows

% off screen stimulus window
stimulus(1) = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);
stimulus(2) = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);

if tracer;
   minFrameDuration = minFrameDuration * 2;
   tracerPaths = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);
end;

screen(stimulus(1), 'FillRect', gray);
screen(stimulus(2), 'FillRect', gray);

% big eraser
screenBlank=screen(MainWindow, 'OpenOffscreenWindow',  gray, screenRect); 

%make some beeps
[beep, samplingRate] = makeBeep(650,.1);
[errbeep, samplingRate] = MakeBeep(850,.11);
% load paths file (includes 	 'practicePaths', 'experimentalPaths', 'rate', 'duration', 'nDisks', 'practiceTrials', 'experimentalTrials', 'practiceLambda', 'experimentalLambda', 'nDirections', 'experimentalStarts', 'practiceStarts', 'predictedMovieFrameDuration');
%load(practicePathsFile);
% predictedMovieFrameDuration = 40; % expected frame duration; if this is wrong, then the trial durationwill be off, as well as the speed
% totalMovieFrames = round(1000*(duration/predictedMovieFrameDuration));
% blankWindow(1) = round(2000/predictedMovieFrameDuration); % blank comes at least 2 seconds after start of tracking
% blankWindow(2) = totalMovieFrames - round(1000/predictedMovieFrameDuration); % blank comes no later than 1 seconds before end of tracking 


% define the stimulus presentation loop
% time is measured in frames, and measures the total stimulus time
% the frame array keeps track of which frame of its trajectory each
% object is in.
loop = {
'  pointer = ballPointer;'
'  while (frame <= trialFrames);'
'     thisFrame = mod(frame - 1, 2) + 1;'
'     lastFrame = mod(frame, 2) + 1;'
'     if moveType == 1;'
'        frameptr = frame;'
'     else;'
'        frameptr = max(lastVisibleFrame, frame);'
'     end;'
'     time1 = GetSecs;'
'     for n = 1:nDisks'
'        if frame > 1;'
'           screen(''CopyWindow'', eraserPointer, stimulus(lastFrame), imageRect, lastPlaceRect{n}, ''transparent'');'
'        end;'
'        placeRect{n} = [trajectory(n,1,frameptr)-imageX  trajectory(n,2,frameptr)-imageY  trajectory(n,1,frameptr)  trajectory(n,2,frameptr)];'
'        screen(''CopyWindow'', pointer, stimulus(thisFrame), imageRect, placeRect{n}, ''transparent'');'
'     end;'
'     if tracer;'
'        screen(''CopyWindow'', tracerPaths, stimulus(thisFrame), [], [], ''transparent'');'
'     end;'
'     screen(MainWindow, ''WaitBlanking'', 1);'
'     screen(''CopyWindow'', stimulus(thisFrame), MainWindow);'
'     frameDuration(frame) = GetSecs - time1;'
'     lastPlaceRect = placeRect;'
'     frame = frame + 1;'
'  end;'
};

if responseType == 2;
   rcColor = 'white';
else
   rcColor = 'darkGray';
end;

pointerNames         = {'ballPointer', 'cueTargetDisk', 'cueDistracterDisk', 'responseCandidateDisk', 'nonResponseCandidateDisk', 'currentSelectionDisk', 'selectedTargetDisk', 'selectedDistracterDisk', 'unselectedTargetDisk'};
diskForegroundColors = {'darkGray',    '[]',            'darkGray',          rcColor,                 'darkGray',                 'white',                'green',              'red',                    'yellow'              };
diskBackgroundColors = {'[]',          '[]',            '[]',                'gray',                  'gray',                     'white',                'green',              'red',                    'yellow'              };
diskOutlineColors    = {'black',       '[]',            'black',             'black',                 'black',                    'black',                'black',              'black',                  'black'               };
for p = 1:(size(pointerNames, 2))
   eval([pointerNames{p}, ' = screen(MainWindow, ''OpenOffScreenWindow'', ', diskBackgroundColors{p}, ', imageRect);']);
   eval(['screen(', pointerNames{p}, ', ''FillOval'',', diskOutlineColors{p}, ', imageRect);']);
   eval(['screen(', pointerNames{p}, ', ''FillOval'',', diskForegroundColors{p}, ', ballRect);']);
end
eraserPointer = screen(MainWindow, 'OpenOffScreenWindow', [], imageRect);
screen(eraserPointer, 'FillOval', gray, imageRect);
if tracer;
   % define tracer window and stimuli
   tracerRect = CenterRect([0 0 2 2], imageRect);
   tracerPointer = screen(MainWindow, 'OpenOffScreenWindow', [], imageRect);
   screen(tracerPointer, 'FillOval', black, tracerRect);
   firstTracerRect = CenterRect([0 0 6 6], imageRect);
   firstTracerPointer = screen(MainWindow, 'OpenOffScreenWindow', [], imageRect);
   screen(firstTracerPointer, 'FillOval', red, firstTracerRect);
end;

pathsFile = practicePathsFile;
load(pathsFile);

% instructions
nDiskString = num2str(nDisks);
nTargetsString = num2str(nTargets);
if nTargets == 1
   verbEnd = 's';
   nounEnd = '';
else
   verbEnd = '';
   nounEnd = 's';
end;

taskInstructionString = {
      'In this experiment, you are asked to keep track of some disks on the screen.';
      ['At the beginning of each trial, you will see ', nDiskString, ' dark gray disks on a medium gray background.'];
      [nTargetsString ' of these disks will blink on and off: These are your targets.'];
      '';
                   };

if nTargets == 1
   taskInstructionString{3} = 'One disk will blink on and off: This is your target.';
end;

if moveType == 1
   taskInstructionString{5} = ['After the target' nounEnd ' stop' verbEnd ' blinking, all the disks will start to move around the screen.'];
   taskInstructionString{6} = 'At some point during the trial, all the disks will become invisible, during which time';
   taskInstructionString{7} = 'they will continue moving, and then reappear.';
else
   taskInstructionString{5} = ['After the target' nounEnd ' stop' verbEnd ' blinking, the disks will remain in place for several seconds.'];
   taskInstructionString{6} = 'At some point during the trial, all the disks will become invisible, during which time';
   taskInstructionString{7} = 'they will move, and then reappear.';
end;

if nTargets == 1
   taskInstructionString{8} = 'Your task is to keep track of the target throughout the trial.';
else
   taskInstructionString{8} = ['Your task is to keep track of the ' nTargetsString ' targets throughout the trial.'];
end;
taskInstructionString{9} = '';

% if conditions == 1 
%    gapInstructionString = {
%          'At some point during the trial, all the disks will briefly become invisible, and then reappear.';
%                    };
% else
%    gapInstructionString = {
%          'One by one, the disks will briefly become invisible and then reappear.';
%                    };
% end

responseInstructionString = {
      'Once the disks stop moving, the arrow cursor will appear.';
      'Use the mouse to click on all of the targets.';
      '';
      'If you click on a target, it will be highlighted in green';
      'and you will hear a low tone.';
      'If you click on a non-target, it will be highlighted in red';
      'and you will hear a high tone.';
      ['After you have clicked on ' nTargetsString ' disks, any targets you missed will blink in yellow.'];
                   };

if responseType == 2
   responseInstructionString{1} = 'Once the disks stop moving, two disks will be highlighted in white,';
   responseInstructionString{2} = 'one target and one non-target.';
   responseInstructionString{3} = 'Using the mouse, select the target from among the two highlighted disks.';
   responseInstructionString{8} = 'If you select a non-target by mistake, the actual target will blink in yellow';
elseif nTargets == 1
   responseInstructionString{2} = 'Use the mouse to click on the target.';
   responseInstructionString{8} = 'If you select a non-target by mistake, the actual target will blink in yellow';
end;

feedbackInstructionString = {
      'After each experimental trial, you will receive feedback'
      'on your performance so far.';
      'The feedback will tell you how you did on the previous trial';
      'and how you have done on all trials for the current block.';
      ['Your goal should be to maintain an overall accuracy of at least ' minimumAccuracy '.'];
                   };

hidecursor;

screen('CopyWindow', screenBlank, MainWindow);
CenterCellText(MainWindow, taskInstructionString, 50);
CenterText('press any key to continue', 0, 250);
FlushEvents('keyDown');
GetChar;
% screen('CopyWindow', screenBlank, MainWindow);
% CenterCellText(MainWindow, gapInstructionString, 50);
% CenterText('press any key to continue', 0, 250);
% FlushEvents('keyDown');
% GetChar;
screen('CopyWindow', screenBlank, MainWindow);
CenterCellText(MainWindow, responseInstructionString, 50);
CenterText('press any key to continue', 0, 250);
FlushEvents('keyDown');
GetChar;
screen('CopyWindow', screenBlank, MainWindow);
% CenterCellText(MainWindow, feedbackInstructionString, 50);
% CenterText('press any key to continue', 0, 250);
% FlushEvents('keyDown');
% GetChar;

slowFlag = 0;

for block = 1:2
   if block == 1
      pathsFile = practicePathsFile;
      blockMessage = ' practice';
      blocktype = 'practice';
      % figure out the max number of repetitions that can fit given the number of
      % trials present in the practice path file:
      %trialTypeList = shuffle(repmat(trialTypes, 1, floor(nTrials / nTrialTypes)));
   else
      pathsFile = experimentalPathsFile;
      blockMessage = ' experimental';
      blocktype = 'exp';
      % Generate and shuffle list of possible trial types:
      %trialTypeList = shuffle(repmat(trialTypes, 1, repetitions));
   end

   load(pathsFile);
   nTargets = nTargets;

   trajectory = paths{1};

   if logging;
      fprintf(logFile, 'trialTypeList = \n');
      fprintf(logFile, '%4.2f ', shiftFactor);
      fprintf(logFile, '\n');
   end;
   
   % 	% check to make sure there are enough frames in the path file:
   % 	if trialDuration > totalMovieFrames;
   % 		clear screen;
   % 		fprintf(1,'ERROR: Trial duration exceeded duration of trajectories in paths file %s.mat\n\n', pathsFile);
   % 		return;
   % 	end;
   % 
   % 	% check to make sure there are enough trials in the paths file:
   % 	if size(trialTypeList, 2) > nTrials
   % 		clear screen;
   % 		fprintf(1, 'ERROR: Number of trials requested (%d) exceeds number of trials in paths file %s.mat (%d)\n\n',...
   % 		        size(trialTypeList, 2), pathsFile, nTrials);
   % 		return;
   % 	end;

   %	nTrials = size(trialTypeList, 2);

   screen('CopyWindow', screenBlank, MainWindow);
   [newX newY] = CenterText(['Press any key to begin ', num2str(nTrials), blockMessage, ' trials']);
   FlushEvents('keyDown');
   GetChar;

   nTracked = zeros(nTrials,1);
   correct = 0;
   
   trials = 1:nTrials;
   if randomizeTrials
      trials = Shuffle(trials);
   end;

   trialcounter = 0;
   for trial = trials
      trialcounter = trialcounter + 1;
      trialString = num2str(trialcounter);
      trialFrames = movieFrames(trial);
      lastVisibleFrame = blankStart(trial) - 1;
      firstVisibleFrame = blankStart(trial) + blankDuration(trial);
      trialShift = shiftAmount(trial);

      %blankWindow(1) = round(2000/predictedMovieFrameDuration); % blank comes at least 2 seconds after start of tracking
      %blankWindow(2) = trialFrames - round(1000/predictedMovieFrameDuration); % blank comes no later than 1 seconds before end of tracking 
      
      timestamp = datestr(now);
      
      if logging;
         fprintf(logFile, 'Starting trial %d\n', trial);
         fprintf(logFile, '  trialFactor = %4.2f\n', trialFactor);
      end;
      
      % % select start time for blank interval
      % startBlankTimeRange = blankWindow(2) - blankWindow(1);
      % if condition == 1
         % % set start of blank to the same frame for all disks
         % startBlankTime = ones(1, nDisks);
         % startBlankTime = startBlankTime * randi(startBlankTimeRange);
      % else
         % % generate different random starting blank frames for each disk
         % startBlankTime = randi(startBlankTimeRange, [1, nDisks]);
      % end
      % startBlankTime = blankWindow(1) + startBlankTime;
      % endBlankTime = startBlankTime + blankDuration;
      % finalFrame = trialFrames; % finalFrame is leftover from dougtrack4
      
      % get coordinates
      trajectory = paths{trial};
      if moveType == 1
         startCoordinates = trajectory(:, :, 1);
      else
         startCoordinates = trajectory(:, :, lastVisibleFrame);
      end;

      %present the stimuli
      hidecursor;
      
      % blank both stimulus screens
      screen('CopyWindow', screenBlank, stimulus(1)); 
      screen('CopyWindow', screenBlank, stimulus(2)); 
      screen('CopyWindow', screenBlank, MainWindow);

      % generate tracers
      if tracer;
         %screen('CopyWindow', screenBlank, tracerPaths,[],[],'transparent');
         screen(tracerPaths, 'Close');
         tracerPaths = screen(MainWindow, 'OpenOffscreenWindow', [], screenRect);
         for d = 1:nDisks;
            if d < nTargets
               color = red;
            else
               color = blue;
            end;
            screen(tracerPaths, 'DrawLine', color, ...
                   trajectory(d, 1, lastVisibleFrame)-imageX/2, (trajectory(d, 2, lastVisibleFrame))-imageY/2, ...
                   trajectory(d, 1, firstVisibleFrame)-imageX/2,      (trajectory(d, 2, firstVisibleFrame))-imageY/2);
            screen('CopyWindow', firstTracerPointer, tracerPaths, imageRect, ...
                   [trajectory(d, 1, lastVisibleFrame)-imageX  trajectory(d, 2, lastVisibleFrame)-imageY ...
                   trajectory(d, 1, lastVisibleFrame)      trajectory(d, 2, lastVisibleFrame)], ...
                   'transparent');
         end;
      end;

      [newX newY] = CenterText(['Press any key to begin trial ', trialString]);
      FlushEvents('keyDown');
      GetChar;
      for a = 1:nDisks
         screen('CopyWindow', ballPointer, stimulus(2), [], [startCoordinates(a,1) - imageX startCoordinates(a,2) - imageY startCoordinates(a,1) startCoordinates(a,2)], 'transparent');
         if a > nTargets
            screen('CopyWindow', cueDistracterDisk, stimulus(1), [], [startCoordinates(a,1) - imageX startCoordinates(a,2) - imageY startCoordinates(a,1) startCoordinates(a,2)], 'transparent');
         else
            screen('CopyWindow', cueTargetDisk, stimulus(1), [], [startCoordinates(a,1) - imageX startCoordinates(a,2) - imageY startCoordinates(a,1) startCoordinates(a,2)], 'transparent');
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
      frameDuration = zeros(trialFrames,1);
      preBlank = ones(1,nDisks);
      frameOffset = zeros(1,nDisks);
      frame = 1;
      
      if logging;
         fprintf(logFile, '  Entering loop...');
      end;
      
      % show the movie:	
      priorityLevel=MaxPriority(MainWindow,'WaitBlanking');
      rush(loop,priorityLevel);
      
      if logging;
         fprintf(logFile, 'done\n');
      end;
      
      %trialDuration = GetSecs - initTime
      averageFrameDuration = sum(frameDuration) / trialFrames * 1000
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
      testFrame = movieFrames(trial);
      
      FlushEvents(['mouseUp'],['mouseDown']);
      ShowCursor([0]);
      % 		screen(MainWindow, 'FillRect', darkGray);
      locationError = ones(1, nTargets);%everything starts as an error
      
      if logging;
         fprintf(logFile, '  Starting response collection...');
      end;
      
      % set up matrix of disks we are actually interested in:
      if responseType == 1
         % full report: click on nTargets disks out of all of them
         respDisks = 1:nDisks;
         respTargets = 1:nTargets;
      elseif responseType == 2
         % 2AFC: hilight 1 target and 1 distracter, and S picks the target
         respDisks = [1, nTargets+1];
         respTargets = 1;
         diskVector(respDisks) = 0;
      else
         fprintf(1, 'Response type of %d is not implemented\n', responseType);
         clear screen;
         return;
      end;
      
      nRespDisks = size(respDisks, 2);
      nRespTargets = size(respTargets, 2);
      respCoordinates = trajectory(respDisks, :, testFrame);
      nClicks = 0;
      selectedDisks = zeros(1, nDisks);
      errorDisks = zeros(1, nDisks);
      clear placeRect;
      
      for d = 1:nDisks
         placeRect{d} = [trajectory(d, 1, testFrame)-imageX  trajectory(d, 2, testFrame)-imageY  trajectory(d,1,testFrame)  trajectory(d,2,testFrame)];
      end;

      while nClicks < nRespTargets
         button = 0;
         while button == 0
            [x, y, button] = getmouse;
            % redraw stimulus
            for d = respDisks
               if selectedDisks(d) == 1
                  screen('CopyWindow', selectedTargetDisk, MainWindow, imageRect, placeRect{d});
               elseif selectedDisks(d) == -1
                  screen('CopyWindow', selectedDistracterDisk, MainWindow, imageRect, placeRect{d});
               elseif IsInRect(x, y, placeRect{d})
                  screen('CopyWindow', currentSelectionDisk, MainWindow, imageRect, placeRect{d});
               else
                  screen('CopyWindow', responseCandidateDisk, MainWindow, imageRect, placeRect{d});
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
         for d = respDisks
            if IsInRect(x, y, placeRect{d})
               if selectedDisks(d) == 0
                  clickedInARect = 1;
                  if any(d == respTargets)
                     screen('CopyWindow', selectedTargetDisk, MainWindow, imageRect, placeRect{d});
                     selectedDisks(d) = 1;
                     snd('play', beep);
                  else
                     screen('CopyWindow', selectedDistracterDisk, MainWindow, imageRect, placeRect{d});
                     snd('play', errbeep);
                     selectedDisks(d) = -1;
                     errorDisks(d) = 1;
                  end
               end; % if selectedDisks(d) == 0
            end; %if IsInRect(x, y, placeRect{d})
         end; %for d = respDisks
         if clickedInARect == 1
            nClicks = nClicks + 1;
         end;
      end % while nClicks...
      
      % if there were any errors, then flash the unselected correct disks:
      if any(errorDisks == 1)
         % run two loops
         for d = 1:nDisks
            if selectedDisks(d) == 1 % selected target
               screen('CopyWindow', selectedTargetDisk, stimulus(2), [], placeRect{d});%, 'transparent');
               screen('CopyWindow', selectedTargetDisk, stimulus(1), [], placeRect{d});%, 'transparent');
            elseif selectedDisks(d) == -1 % selected distracter
               screen('CopyWindow', selectedDistracterDisk, stimulus(2), [], placeRect{d});%, 'transparent');
               screen('CopyWindow', selectedDistracterDisk, stimulus(1), [], placeRect{d});%, 'transparent');
            else % disk was not selected
               if any(d == respTargets) % unselected target
                  screen('CopyWindow', responseCandidateDisk, stimulus(2), [], placeRect{d});%, 'transparent');
                  screen('CopyWindow', unselectedTargetDisk, stimulus(1), [], placeRect{d});%, 'transparent');
               elseif any(d == respDisks) % unselected distracter
                  screen('CopyWindow', responseCandidateDisk, stimulus(2), [], placeRect{d});%, 'transparent');
                  screen('CopyWindow', responseCandidateDisk, stimulus(1), [], placeRect{d});%, 'transparent');
               else % unselected and not a response candidate
                  screen('CopyWindow', nonResponseCandidateDisk, stimulus(2), [], placeRect{d});%, 'transparent');
                  screen('CopyWindow', nonResponseCandidateDisk, stimulus(1), [], placeRect{d});%, 'transparent');
                  %screen('CopyWindow', ballPointer, stimulus(2), [], placeRect{d});%, 'transparent');
                  %screen('CopyWindow', ballPointer, stimulus(1), [], placeRect{d});%, 'transparent');
               end;
            end; % if selectedDisks(d) == 1
         end; % for d = 1:nDisks
         %flash the unselected correct balls 3 times
         for flash = 1:6
            screen('CopyWindow', stimulus(mod(flash,2)+1), MainWindow);
            WaitSecs(.333);
         end;
      end;
      
      if logging;
         fprintf(logFile, 'done\n');
      end;
      
      error = max(errorDisks);
      nErrors = sum(errorDisks);
      % save data
      dataFile = fopen(dataFileName, 'a');
      %header = 'sub,time,pracfile,pathfile,blocktype,movetype,trialdur,blankdur,trial,line,speed,shift,ndisks,ntargets,framedur,error,nerrors\n'
      count = fprintf(dataFile, ...
                      '%s,%s,%s,%s,%s,%d,%d,%d,%d,%d,%d,%4.2f,%d,%d,%6.4f,%d,%d\n', ...
                      subject, timestamp, practicePathsFile, experimentalPathsFile, blocktype, ...
                      moveType, trialFrames, blankDuration(trial), ...
                      trialcounter, trial, movementRate(trial), shiftFactor(trial), nDisks, ...
                      nTargets, averageFrameDuration, error, nErrors);
      fclose(dataFile);
      
      [newX newY] = CenterText('Press any key to continue', 0, 0, black);
      FlushEvents('keyDown');
      GetChar;

      nTracked(trialcounter) = nTargets - nErrors;
      correct = correct + (1 - error);
      
      feedbackString = {['Last Trial: ' num2str(nTracked(trialcounter)) '/' num2str(nTargets) ' correct'];
                        ['Average of: ' num2str(sum(nTracked(1:trialcounter))/trialcounter,2) '/' num2str(nTargets) ' correct'];
                        %' ';
                        %['All ' num2str(nTargets) ' correct on ' num2str(100*correct/trialcounter,'%1.0f%%') ' of trials'];
                        ' ';
                        ' ';
                        'Press any key to continue'
                       };

      screen('CopyWindow', screenBlank, MainWindow);
      [newX newY] = CenterCellText(MainWindow, feedbackString, 30);
      FlushEvents('keyDown');
      GetChar;
   end % trial loop
   
   if slowFlag > 0
      break;
   end;
   
end % block loop

if logging;
	fprintf(logFile, 'Finished on %s\n', datestr(now));
	fclose(logFile);
end;

clear screen;
