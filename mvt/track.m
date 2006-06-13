function track()

%%% track:
%%% presents trials in the Multiple Object Tracking experiments. Uses path files 
%%% generated by generator.m
%%% Authors: David Fencsik (based on file by Todd Horowitz)
%%%
%%% $LastChangedDate$

experiment = 'StopTrack07';
Version = '$Rev$';

%%% input dialog %%%
dlgParam = {'subject'      , 'Subject initials'               , 'xxx';
            'pathFile'     , 'Path file name'                 , 'StopTrack07PathsA';
            'pBlock'       , 'Practice block (1 = yes)'       , '0';
           };
param = inputdlg(dlgParam(:, 2), ['Experiment Parameters'], 1, dlgParam(:, 3));
if size(param) < 1
   return
end
for a = 1:length(param)
   p = param{a};
   n = str2num(p);
   if isempty(n)
      str = 'p';
   else
      str = 'n';
   end
   eval([dlgParam{a, 1} ' = ' str ';']);
end

%%% set remaining parameters
% pathFile = 'StopTrack07Paths';
% moveTypeList = [0 1];
probeTargetList = [0 1];
correctDAC = 0;
revealTargets = 1;

%%% set some fixed parameters
asynchronous = 0;
shift = 1;
pauseEvery = 50;
pauseMin = 375; % frames

%%% block settings
if pBlock
   pTrials = 0;
   xTrials = 50;
   moveTypeList = 1;
   nTargetsList = 4;
else
   pTrials = 12;
   xTrials = 288;
   moveTypeList = [0 1];
   nTargetsList = [2 3 4];
end

%%% initialize RNG
seed = sum(100*clock);
rand('state', seed);

%%% fix up Version string (this removes the extra characters from SVN)
Version = Version(7:length(Version)-2);

%%% Define response keys
respTarget = 40; % ' key (right-hand side)
respDistractor  =  1; % a key (left-hand side)
allowedResponses = [respTarget, respDistractor];

%%% load path file, extract information, set up path order, and check consistency
load(pathFile);
[nDisks, nCoord, nPaths] = size(startPositions);
nTrials = pTrials + xTrials;
if nTrials > nPaths
   error(sprintf('path file %s cannot support %d total trials; max is %d', pathFile, nTrials, nPaths));
end
if durationFlag == 2  % duration levels are balanced
   durList = unique(pathDurations);
   nDur = length(durList);
   % figure out how many paths per duration level
   nPathsPerDuration = nPaths / nDur; % this should be an integer
   if nPathsPerDuration ~= round(nPathsPerDuration)
      error(sprintf('path file %s has unbalanced number of paths per duration'), pathFile);
   end
   % generate shuffled list of paths for each duration level
   pathList = zeros(nDur, nPathsPerDuration);
   for d = 1:nDur
      pathList(d, :) = Shuffle(find(pathDurations == durList(d)))';
   end
   % set up list of possibly balanced duration levels for practice trials
   if pTrials > 0
      durIndexPrac = repmat(1:nDur, [1, ceil(pTrials / nDur)]);
   end
   % set up counters for each duration level
   pathCounter = ones(nDur, 1);
else % duration levels are not balanced
   % randomize paths
   nDur = 1;
   pathList = randperm(nPaths);
   pathCounter = 1;
end
durIndexList = 1:nDur;

%%% other info
dataFileName = sprintf('%sData-%s.txt', experiment, subject);
computer = strtok(pwd, ':');
blocktime = now;

screenNumber = max(Screen('Screens'));
winMain = Screen(screenNumber, 'OpenWindow', [], [], 8);
rectScreen = Screen(winMain, 'Rect');
[centerX, centerY] = RectCenter(rectScreen);
refreshDuration = 1 / Screen(winMain, 'FrameRate', []); % sec/frame
rectDisplay = CenterRect(rectDisplay, rectScreen);

HideCursor;

%%% Define colors %%%
colWhite = WhiteIndex(winMain);
colBlack = BlackIndex(winMain);
colTransparent = colWhite;
colOffWhite = abs(colWhite - 1); % abs corrects for white being 0; abs(0 - 1) = 1, abs(255 - 1) = 254
colRed   = 10;
colBlue  = 11;
colGreen = 12;
colYellow = 13;
colLightGray = 14;
clut = Screen(winMain, 'GetClut');
clut(colRed + 1, :)        = [255   0   0];
clut(colGreen + 1, :)      = [  0 255   0];
clut(colBlue + 1, :)       = [  0   0 255];
clut(colYellow + 1, :)     = [255 255   0];
clut(colLightGray + 1, :)  = [191 191 191];
if correctDAC
   LoadClut(winMain, clut*1023/255);
else
   LoadClut(winMain, clut);
end;
colBackground = colLightGray;
colForeground = colBlack;
colText = colBlack;
colInstructions = colBlack;
colDisk = colOffWhite;
colDiskCorrect = colGreen;
colDiskError = colRed;
colDiskIndicator = colYellow;
colDiskProbe = colRed;
colDiskBorder = colBlack;
colFixation = colGreen;

Screen(winMain, 'FillRect', colBackground);

Screen(winMain, 'TextFont', 'Monaco');
Screen(winMain, 'TextSize', 18);

%%% define some tones %%%
beepCorrect = MakeBeep(1000, .01);
beepError   = MakeBeep(880, .2);
Snd('Play', beepCorrect);


%%% Define display windows %%%
winDisplayBlank = Screen(winMain, 'OpenOffscreenWindow', colBackground, rectDisplay);
Screen(winDisplayBlank, 'FrameRect', colForeground, [], 1, 1);
winDisplay = Screen(winMain, 'OpenOffscreenWindow', colBackground, rectDisplay);
Screen('CopyWindow', winDisplayBlank, winDisplay);
winDB = zeros(2, 1);
for d = 1:2
   winDB(d) = Screen(winMain, 'OpenOffscreenWindow', colBackground, rectDisplay);
   Screen('CopyWindow', winDisplayBlank, winDB(d));
end


%%% Define disks %%%
diskDiameter = diskRadius * 2;
border = 2;
rectDisk    = [0 0 diskDiameter diskDiameter];
diskNames        = {'', 'Correct', 'Error', 'Indicator', 'MouseOver', 'Blank', 'Probe'};
diskBackgrounds  = {'[]', '[]', '[]', '[]', 'colOffWhite', 'colTransparent', '[]'};
diskColors       = {'colDisk', 'colDiskCorrect', 'colDiskError', 'colDiskIndicator', 'colTransparent', 'colTransparent', 'colDiskProbe'};
diskBorder       = {'border', 'border', 'border', 'border', '[]', '[]', 'border'};
diskBorderColors = {'colDiskBorder', 'colDiskBorder', 'colDiskBorder', 'colDiskBorder', 'colTransparent', 'colTransparent', 'colDiskBorder'};
for d = 1:length(diskNames)
   name = sprintf('winDisk%s', diskNames{d});
   eval(sprintf('%s = Screen(winMain, ''OpenOffScreenWindow'', [], rectDisk);', name));
   if ~isempty(diskBackgrounds{d})
      eval(sprintf('Screen(%s, ''FillRect'', %s);', name, diskBackgrounds{d}));
   end
   eval(sprintf('Screen(%s, ''FillOval'', %s);', name, diskColors{d}));
   if ~isempty(diskBorder{d}) > 0 & ~isempty(diskBorderColors{d})
      eval(sprintf('Screen(%s, ''FrameOval'', %s, [], %s, %s);', ...
                   name, diskBorderColors{d}, diskBorder{d}, diskBorder{d}));
   end
end



%%% Define animation loop
%%% 1. Show initial display
%%% 2. Cue targets
%%% 3. Pause
%%% 4. Tracking interval
animationLoop = {
   'startTime = GetSecs;'
   'Screen(''CopyWindow'', winDB(1), winMain, [], rectDisplay);'
   'Screen(winMain, ''WaitBlanking'', round(.5 / refreshDuration));'
   'for f = [2 1 2 1 2 1 2 1];'
   '   Screen(''CopyWindow'', winDB(f), winMain, [], rectDisplay);'
   '   Screen(winMain, ''WaitBlanking'', round(1/3 / refreshDuration));'
   'end;'
   'Screen(''CopyWindow'', winDB(1), winMain, [], rectDisplay);'
   'Screen(winMain, ''WaitBlanking'', round(.25 / refreshDuration));'
   'for f = 1:nFrames;'
   '   Screen(''CopyWindow'', winDisplayBlank, winDisplay);'
   '   if f < gapOnset | f >= gapOffset;'
   '      for d = drawingOrder;'
   '         Screen(''CopyWindow'', diskPointer(d), winDisplay, [], rectStim(d, :, f), ''transparent'');'
   '      end;'
   '   end;'
   '   Screen(''CopyWindow'', winDisplay, winMain, [], rectDisplay);'
   '   Screen(winMain, ''WaitBlanking'');'
   '   frameDisplayTime(f) = GetSecs;'
   'end;'
   'endTime = GetSecs;'
                };


%%% balance independent variables
NumberOfTrials = xTrials;
IVs = {'probeTarget'   , probeTargetList;
       'nTargets'      , nTargetsList;
       'moveType'      , moveTypeList;
       'durIndex'      , durIndexList;
      };
nVariables = size(IVs, 1);
varLength = zeros(nVariables, 1);
listLength = 1;
for v = 1:nVariables
   varLength(v) = length(IVs{v, 2});
   listLength = listLength * varLength(v);
end
nRepetitions = ceil(NumberOfTrials / listLength);
len1 = listLength;
len2 = 1;
[dummy, index] = sort(rand(listLength * nRepetitions, 1));
for v = 1:nVariables
   len1 = len1 / varLength(v);
   eval([IVs{v, 1} ' = repmat(reshape(repmat(IVs{v, 2}, len1, len2), listLength, 1), nRepetitions, 1);']);
   eval([IVs{v, 1} ' = ' IVs{v, 1} '(index);']);
   len2 = len2 * varLength(v);
end
if listLength * nRepetitions ~= NumberOfTrials
   warning('unbalanced design');
end
clear NumberOfTrials IVs nVariables varLength listLength nRepetitions v dummy len1 len2 index;

% %%% Prepare/present instructions
% instr = cell(2, 1);

% if nTargets >= 1 & nTargets <= 9
%    nTargetsString = sprintf('%d', nTargets);
% else
%    error(sprintf('%d targets not supported', nTargets));
% end

% if moveType == 0
%    instr{1} = {'Instructions';
%                '';
%                'In this experiment, you need to keep track of some disks on ';
%                'the screen.  At the start of each trial, you will see 10    ';
%                ['white disks on a gray background.  ' nTargetsString ' of these disks will    '];
%                'blink on and off: these are your targets.  After the disks  ';
%                'stop blinking, the disks will stay in place for several     ';
%                'seconds. Your task is to keep track of the targets          ';
%                'throughout the trial.                                       ';
%                '';
%               };
% else
%    instr{1} = {'Instructions';
%                '';
%                'In this experiment, you need to keep track of some disks on ';
%                'the screen.  At the start of each trial, you will see 10    ';
%                ['white disks on a gray background.  ' nTargetsString ' of these disks will    '];
%                'blink on and off: these are your targets.  After the disks  ';
%                'stop blinking, all the disks will begin to move around the  ';
%                'screen.  Your task is to keep track of the targets          ';
%                'throughout the trial.                                       ';
%                '';
%               };
% end
% instr{2} = {'Instructions';
%             '';
%             'Once the trial ends, the arrow cursor will appear.  Use the ';
%             'mouse to click on each of the targets.  If you click on a   ';
%             'target, it will be highlighted in green and you will hear a ';
%             'click.  If you click on a non-target, it will be highlighted';
%            ['in red and you will hear a beep.  Once you have selected ' nTargetsString '  '];
%             'disks, any targets that you missed will blink in yellow.    ';
%             '';
%             '';
%            };

% for a = [1 2]
%    if ~isempty(instr{a})
%       Screen(winMain, 'FillRect', colBackground);
%       CenterCellText(winMain, instr{a}, colInstructions, 30);
%       CenterText(winMain, 'Press any key to continue', colInstructions, 0, 250);
%       WaitForButtonPress(winMain, 1);
%    end
% end

Screen(winMain, 'FillRect', colBackground);

for trial = 1:nTrials
   trialtime = datestr(now);
   
   Screen('CopyWindow', winDisplayBlank, winMain, [], rectDisplay);
   CenterText(winMain, 'Please wait...', colText);
   
   prepStart = GetSecs;

   %%% get trial parameters
   if trial <= pTrials
      prac = 1;
      trialIndex = Randi(xTrials);
   else
      prac = 0;
      trialIndex = trial - pTrials;
   end

   %%% determine trial duration index
   if durationFlag == 2 & prac == 1
      idur = durIndexPrac(trial);
   else
      idur = durIndex(trialIndex);
   end

   if pBlock
      prac = 1;
   end

   %%% pick trial path
   path = pathList(idur, pathCounter(idur));
   pathCounter(idur) = pathCounter(idur) + 1;

   %%% basic trial info
   nFrames = pathDurations(path);
   pos = startPositions(:, :, path);
   delta = startVelocities(:, :, path);
   gapOnset = nFrames - blankDuration;
   gapOffset = nFrames;
   rectStim = zeros(nDisks, 4, nFrames);

   %%% generate trajectories
   for f = 1:nFrames

      rectStim(:, :, f) = [pos - diskRadius, pos + diskRadius];

      % test future positions for bouncing
      next = pos + delta;
      bounceX = (next(:, 1) < rectBoundary(RectLeft) | next(:, 1) > rectBoundary(RectRight))';
      bounceY = (next(:, 2) < rectBoundary(RectTop) | next(:, 2) > rectBoundary(RectBottom))';
      if any(bounceX), delta(bounceX, 1) = -1 * delta(bounceX, 1); end
      if any(bounceY), delta(bounceY, 2) = -1 * delta(bounceY, 2); end

      pos = pos + delta;

   end

   %%% if static trial, then set all pre-gap frames to be identical to the pre-gap frame
   if moveType(trialIndex) == 0
      rectStim(:, :, 1:(gapOnset-2)) = repmat(rectStim(:, :, gapOnset - 1), [1, 1, gapOnset - 2]);
   end
   
   %%% pick probe
   if probeTarget(trialIndex)
      probeDisk = Randi(nTargets(trialIndex));
   else
      probeDisk = Randi(nDisks - nTargets(trialIndex)) + nTargets(trialIndex);
   end

   diskPointer = repmat(winDisk, [nDisks, 1]);
   drawingOrder = randperm(nDisks);

   %%% Prepare cue displays
   for d = 1:2
      Screen('CopyWindow', winDisplayBlank, winDB(d));
   end
   for d = drawingOrder
      Screen('CopyWindow', diskPointer(d), winDB(1), rectDisk, rectStim(d, :, 1), 'transparent');
      if d > nTargets(trialIndex)
         Screen('CopyWindow', diskPointer(d), winDB(2), rectDisk, rectStim(d, :, 1), 'transparent');
      end
   end
   
   prepDur = GetSecs - prepStart;

   Screen('CopyWindow', winDisplayBlank, winMain, [], rectDisplay);
   CenterText(winMain, sprintf('Press any key to begin trial %d of %d', trial, nTrials), colText);
   WaitForButtonPress(winMain, 1);
   
   %%% Initialize some variables and pre-load some functions into memory
   Screen(winMain, 'WaitBlanking');
   startTime = GetSecs;
   endTime = 0;
   frameDisplayTime = zeros(nFrames, 1);
   d = 0; f = 0;
   tProbeOnset = 0;
   keyDown = 0; tResponseOnset = 0; tResponseOffset = 0; keyCode = 0;

   %%% Display animation loop
   %Rush(animationLoop, 0);
   Rush(animationLoop, MaxPriority(winMain, 'WaitBlanking', 'GetSecs'));

   Screen(winMain, 'WaitBlanking', 8);
   Screen('CopyWindow', winDiskProbe, winDisplay, [], rectStim(probeDisk, :, nFrames), 'transparent');
   Screen('CopyWindow', winDisplay, winMain, [], rectDisplay);
   Screen(winMain, 'WaitBlanking');
   tProbeOnset = GetSecs;
   while 1
      [keyDown, tResponseOnset, keyCode] = KbCheck;
      if keyDown
         break;
      end
   end
   while keyDown
      keyDown = KbCheck;
   end
   tResponseOffset = GetSecs;
   
   respRT = tResponseOnset - tProbeOnset;
   respDur = tResponseOffset - tResponseOnset;
   respCode = find(keyCode);
   if length(respCode) > 1
      respString = 'multiple';
      respAcc = -1;
   elseif respCode == respTarget
      respString = 'target';
      if probeTarget(trialIndex)
         respAcc = 1;
      else
         respAcc = 0;
      end
   elseif respCode == respDistractor
      respString = 'distractor';
      if probeTarget(trialIndex)
         respAcc = 0;
      else
         respAcc = 1;
      end
   else
      respString = fprintf('%d', respCode);
      respAcc = -1;
   end
   
   if respAcc == 1
      Snd('Play', beepCorrect);
      feedbackString = 'CORRECT';
      colFeedback = colBlue;
   else
      Snd('Play', beepError);
      feedbackString = 'ERROR';
      colFeedback = colRed;
   end
   CenterText(winMain, feedbackString, colFeedback);

   frameDurations = diff(frameDisplayTime);

   dataFile = fopen(dataFileName, 'r');
   if dataFile == -1
      header = ['exp,sub,code,revision,computer,blocktime,pathfile,path,prac,trial,trialtime,' ...
                'nframes,refreshdur,ndisks,ntargets,targ,probe,blankdur,asynch,shift,move,' ...
                'response,rt,dur,acc,prepdur,meanframedur,minframedur,maxframedur'];
   else
      fclose(dataFile);
      header = [];
   end
   dataFile = fopen(dataFileName, 'a');
   if dataFile == -1
      error(sprintf('cannot open data file %s for writing', dataFileName));
   end
   if ~isempty(header)
      fprintf(dataFile, '%s\n', header);
   end
   %                  %exp        %computer      %trial         %nDisks        %asynch  %response               %framedurs
   fprintf(dataFile, '%s,%s,%s,%s,%s,%f,%s,%d,%d,%d,%s,%d,%0.3f,%d,%d,%d,%d,%d,%d,%d,%d,%s,%0.0f,%0.0f,%d,%0.3f,%0.3f,%0.3f,%0.3f\n', ...
           experiment, subject, mfilename, Version, computer, blocktime, pathFile, path, prac, trial, trialtime, ...
           nFrames, refreshDuration * 1000, nDisks, nTargets(trialIndex), ...
           probeTarget(trialIndex), probeDisk, blankDuration, ...
           asynchronous, shift, moveType(trialIndex),  ...
           respString, respRT * 1000, respDur * 1000, respAcc, ...
           prepDur * 1000, mean(frameDurations) * 1000, ...
           min(frameDurations) * 1000, max(frameDurations) * 1000);
   fclose(dataFile);

   if revealTargets & ~(nTargets(trialIndex) == 1 & probeTarget(trialIndex))
      for d = 1:2
         Screen('CopyWindow', winDisplayBlank, winDB(d));
      end
      for d = drawingOrder
         if d == probeDisk & d <= nTargets(trialIndex)
            Screen('CopyWindow', winDiskProbe, winDB(2), [], rectStim(d, :, nFrames), 'transparent');
         elseif d <= nTargets(trialIndex)
            Screen('CopyWindow', winDisk, winDB(2), [], rectStim(d, :, nFrames), 'transparent');
         elseif d == probeDisk
            Screen('CopyWindow', winDiskProbe, winDB(1), [], rectStim(d, :, nFrames), 'transparent');
            Screen('CopyWindow', winDiskProbe, winDB(2), [], rectStim(d, :, nFrames), 'transparent');
         else
            Screen('CopyWindow', winDisk, winDB(1), [], rectStim(d, :, nFrames), 'transparent');
            Screen('CopyWindow', winDisk, winDB(2), [], rectStim(d, :, nFrames), 'transparent');
         end
      end

      Screen(winMain, 'WaitBlanking', round(.5 / refreshDuration));

      for d = [1 2 1 2 1 2]
         Screen('CopyWindow', winDB(d), winMain, [], rectDisplay);
         CenterText(winMain, feedbackString, colFeedback);
         Screen(winMain, 'WaitBlanking', round(.2 / refreshDuration));
      end
   end

   WaitSecs(.1);
   CenterText(winMain, 'Press any key to continue', colText, 0, 50);
   WaitForButtonPress(winMain, 1);

   Screen('CopyWindow', winDisplayBlank, winMain, [], rectDisplay);
   
   % pause every N trials, unless there's only one or no trials remaining
   if mod(trial, pauseEvery) == 0 & nTrials - trial > 1
      Screen('CopyWindow', winDisplayBlank, winMain, [], rectDisplay);
      oldFontSize = Screen(winMain, 'TextSize', 24);
      oldFontStyle = Screen(winMain, 'TextStyle', 1); % set to bold
      CenterText(winMain, 'Please take a short break...', colText, 0, -50);
      t1 = Screen(winMain, 'WaitBlanking', pauseMin);
      CenterText(winMain, 'Press any button to continue', colText, 0, 50);
      WaitForButtonPress(winMain, 1);
      Screen(winMain, 'TextSize', oldFontSize);
      Screen(winMain, 'TextStyle', oldFontStyle);
      while KbCheck; end
      Screen('CopyWindow', winDisplayBlank, winMain, [], rectDisplay);
   end

end

Screen(winMain, 'FillRect', colBackground);
mesg = {'Block is complete.'; 'Please inform the experimenter.'};
CenterCellText(winMain, mesg, colInstructions, 40);
FlushEvents('keyDown');
GetChar;

Screen('CloseAll');



function WaitForButtonPress (win, kbFlag)

if nargin < 2
   kbFlag = 0;
end

if kbFlag
   % keyboard
   FlushEvents('keyDown');
   keyDown = KbCheck;
   while keyDown
      keyDown = KbCheck;
      WaitSecs(.01);
   end
   while ~keyDown
      keyDown = KbCheck;
      WaitSecs(.01);
   end
else
   % mouse press
   FlushEvents('mouseUp', 'mouseDown');
   [x, y, button] = GetMouse(win);
   while any(button)
      [x, y, button] = GetMouse(win);
      WaitSecs(.01);
   end
   while ~any(button)
      [x, y, button] = GetMouse(win);
      WaitSecs(.01);
   end
end



function [newX, newY] = CenterText (window, message, color, xoffset, yoffset)

if nargin < 2
   error([mfilename ' requires at least two arguments']);
end
if nargin < 3 | isempty(color)
   color = [];
end
if nargin < 4 | isempty(xoffset)
   xoffset = 0;
end
if nargin < 5 | isempty(yoffset)
   yoffset = 0;
end

windowRect = screen(window, 'Rect');
width = screen(window, 'TextWidth', message);
[newX, newY] = screen(window, 'DrawText', message, ((windowRect(3)/2)-(width/2))+xoffset, (windowRect(4)/2)+yoffset, color);


function [newX, newY] = CenterCellText(window, messages, color, spacing)

if nargin < 2
   error([mfilename ' requires at least two arguments']);
end
if nargin < 3 | isempty(color)
   color = [];
end
if nargin < 4 | isempty(spacing)
   spacing = 20;
end

lines = length (messages);
middleLine = round(lines/2);
yOffset = spacing*((1:lines)-middleLine);
for y = 1:lines
   [newX, newY] = CenterText(window, messages{y}, color, 0, yOffset(y));
end
