function pathsFile = stGenerator (sInitial)

% stGenerator:
% generates a set of trajectories for use with ShiftTrack experiments
% Authors: David Fencsik (based on file by Todd Horowitz)
%
% $Id: generator.m,v 1.12 2004/01/21 21:21:55 fencsik Exp $

% Modified by David Fencsik
% started  9/29/2003
% based on tGenerator6c of  9/15/2003
% version of  01/06/2004

% Fix within-block design so certain variables can be implemented within-block or
% between-block. Each of these variables has a WB version that, if non-empty, is
% manipulated within-block.

starttime = clock;
   
% Scrap this:
% withinBlock = 0; % 1 = within-block design, each prefix gets R repetitions of each trial
%                  % type (not implemented); 
%                  % 0 = between-block design, each prefix gets R
%                  % repetitions of one trial type

% NOT IMPLEMENTED
movingCue = 1; % 0 = objects are cued in first frame and not handled by this file.  
               % 1 = objects begin moving at cuingRate with cuingNoise for
               % cuingDuration, then switch to standard movement rates

subjects = 1; % e.g, 1:10 [ 2 7 11] 

% Possible trial durations, in seconds
minTrialDuration = 5;
maxTrialDuration = 8;

nDisks = 8;
nTargets = 4;

% each variable contains block-level info. Thus, for trialTypes, there should be one
% array for each block; for prefix, there should be one value per block; etc.

trialTypes = {repmat([1;2], [2 1]); 
              repmat([1;2], [5 1])};
%reappearanceOffset = [1.0];
prefix = {'testP'; 'testR'}; %{'train';'a';'b';'c'}; 

blankDurations = {23; 23};
movementRates = {9; 9};
movementNoises = {0; 0};

bufferZone = 50;
preBlankRect = [0 0 1024 768];

clear screen;


% major variables

% timing variables
predictedMovieFrameDuration = 13.33;
maxMovieFrames = ceil(maxTrialDuration*1000/predictedMovieFrameDuration);
minMovieFrames = ceil(minTrialDuration*1000/predictedMovieFrameDuration);
slackDuration = maxTrialDuration - minTrialDuration;
slackFrames = ceil(slackDuration*1000/predictedMovieFrameDuration);


% size/distance variables
%MainWindow = screen(0, 'OpenWindow', [], [], 8);
imageX=35;
imageY=35;
imageRect = [0 0 imageX imageY];
screenX = 1024;
screenY = 768;
screenRect = [0 0 screenX screenY];
%hz = round(screen(MainWindow, 'FrameRate'));
edgeZone = imageY;


% movement variables
global fullCircle halfCircle;
fullCircle = 24;
halfCircle = 12;
% increment = (rate*30)*(frameDuration);
degreesPerDirection = 360/24;

maxReplacements = 100; % how many times we can replace a single disk
maxRestarts = 1000; % how many times we can restart placement of all disks
tau = clock;
rand('state',sum(100*tau));
state=rand('state');


% ensure that pre-gap rectangle is not too big and is centered within the screenRect
% preBlankRect(1) = preBlankRect(1) + edgeZone;
% preBlankRect(2) = preBlankRect(2) + edgeZone;
% preBlankRect(3) = preBlankRect(3) - edgeZone;
% preBlankRect(4) = preBlankRect(4) - edgeZone;
if preBlankRect(3) - preBlankRect(1) > (screenX - 2 * edgeZone)
   preBlankRect(1) = edgeZone;
   preBlankRect(3) = screenX - edgeZone;
end;
if preBlankRect(4) - preBlankRect(2) > (screenY - 2 * edgeZone)
   preBlankRect(2) = edgeZone;
   preBlankRect(4) = screenY - edgeZone;
end;
preBlankRect = CenterRect(preBlankRect, screenRect)



% startCoordinates = zeros(nBalls, 2);
xdim = 5; ydim = 6;% a 5 by 6 grid of positions
windowX = 100;windowY = 100;% each cell is 125 by 125 pixels
cxy = newGrids(xdim, ydim, windowX, windowY, screenRect);


nBlocks = size(trialTypes, 1);
badPath = 1;
for sub = subjects
   %	initTime = GetSecs;
   restarts = 0;
   
   for block = 1:nBlocks
      
      trialType = Shuffle(trialTypes{block});
      nTrials = size(trialType, 1);
      
      filename = [prefix{block} num2str(sub)];
      blankDuration = blankDurations{block};
      movementRate = movementRates{block};
      movementNoise = movementNoises{block};
      
      targetMagnitude = (movementRate*30)*(predictedMovieFrameDuration)/1000
      targetNoiseMagnitude = (movementNoise*30)*(predictedMovieFrameDuration)/1000;
      %distractorMagnitude = (distractorRate*30)*(predictedMovieFrameDuration);
      %distractorNoiseMagnitude = (distractorNoise*30)*(predictedMovieFrameDuration);

      % use kills to keep track of where problems are occuring
      kills = zeros(1,10);
      badTurns = 0;
      movieFrames = ones(1, nTrials) * maxMovieFrames;

      for trial = 1:nTrials

         % Procedure for new design: In which a distractor reappears in the pre-gap
         % location of a target.
         %  3. Determine the trial type:
         %   % 1 = Standard shiftrack: all disks disappear and reappear apart.
         %   % 2 = Distracter shiftrack: one distracter reappears where a target
         %   %     disappeared from.
         % 10. Pick the pre-gap frame (t0) during the tracking interval (t0).
         % 20. Place all the disks with some minimum inter-stimulus distance and a
         %     random trajectory such that it will not reappear in another disk's
         %     pre-gap location. If it does, pick a new location/trajectory, with a
         %     limit for the number of times we can pick again, in which case we
         %     restart step 20.
         % 30. If the trial type requires it, set up the first distracter with an
         %     appropriate location/trajectory. Otherwise, proceed to next step.
         % 40. Move disks backwards along their trajectories until the minDuration
         %     frame is reached.
         % 50. If there is some minimum distance between disks, then stop; otherwise,
         %     continue moving objects along their trajectory until they are a minimum
         %     distance apart or the maximum duration is reached, in which case we
         %     restart step 20, with a limit for how many times we do this.
         % 60. Set positions to some off-screen value during blank interval.
         % 70. Disks reappear at appropriate offsets after blank interval.
         % 80. Move disks along trajectories until the final frame. If they are less
         %     than a minimum distance apart, then restart at 20.
         % 90. Done.

         % Determine the first invisible blank-interval frame: The gap can occur at 
         % least 2 seconds after the start of tracking but no later than 1 second 
         % before the end of tracking:
         blankWindow(1) = round(2000/predictedMovieFrameDuration);
         blankWindow(2) = minMovieFrames - round(1000/predictedMovieFrameDuration);
         blankInterval(trial,1) = Randi(blankWindow(2) - blankWindow(1)) + blankWindow(1);
         blankInterval(trial,2) = blankInterval(trial,1) + blankDuration;

         % compute trajectories
         trajectory = zeros(nDisks, 2, movieFrames(trial));
         % set up vectors of individual signal and noise magnitudes
         magnitude = ones(nDisks, 1) * targetMagnitude; 
         noiseMagnitude = ones(nDisks, 1) * targetNoiseMagnitude; 

         restarts = 0;
         badPath = 1;
         while badPath
            badPath = 0;
            % place each disk and give it a direction, and make sure it
            % is both a minimum distance from other disks and will not reappear too
            % close to any target disk after the blank interval.
            preBlankCoordinates = zeros(nDisks, 2);
            preBlankDirections = zeros(nDisks, 1)
            postBlankCoordinates = zeros(nDisks, 2);
            postBlankDirections = zeros(nDisks, 1)
            for disk = 1:nDisks
               replacements = 0;
               badPlacement = 1;
               while badPlacement
                  badPlacement = 0;
                  x = Randi(preBlankRect(3) - preBlankRect(1), 1);
                  y = Randi(preBlankRect(4) - preBlankRect(2), 1);
                  preBlankCoordinates(disk, :) = [x y];

                  if disk > 1
                     % ensure this disk is far enough from the others
                     coord = preBlankCoordinates(1:(disk-1), :);
                     if any(sqrt((x - coord(:,1)).^2 + (y - coord(:,2)).^2) < bufferZone)
                        % this disk is too close to another
                        badPlacement = 1;
                        kills(1) = kills(1) + 1;
                     end;
                  end;
                     
                  if ~badPlacement
                     preBlankDirections(disk) = Randi(fullCircle, 1);

                     % determine reappearance location of the disk
                     oldCoordinates = preBlankCoordinates(disk,:);
                     direction = preBlankDirections(disk);
                     frame = blankInterval(trial,1);
                     while frame <= blankInterval(trial,2) % go up through the first post-blank frame
                        %% calculate new coordinates and then check for disks going out of bounds
                        for i = 1:3
                           [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0);
                           newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude);
                           status = OutOfBounds(newCoordinates, screenRect, edgeZone);
                           if status ~= 0
                              direction = BounceOffWall(direction, status);
                              badPlacement = 1;
                           else
                              badPlacement = 0;
                              break;
                           end;
                        end;
                     end;
                     if badPlacement
                        kills(2) = kills(2) + 1;
                     else
                        postBlankCoordinates(disk) = newCoordinates;
                        postBlankDirections(disk) = direction;
                        
                        % check to make sure this doesn't reappear too close to any
                        % (other) targets' disappearance locations
                        if disk > 1
                           x = postBlankCoordinates(disk, 1); y = postBlankCoordinates(disk,2);
                           coord = preBlankCoordintes(1:(min(nTargets, disk-1)), :);
                           if disk <= nTargets && ... 
                                      any(sqrt((x - coord(:,1)).^2 + (y- coord(:,2).^2)) < bufferZone)
                              % this disk reappears too close to some (other) target's disappearance location
                              badPlacement = 1;
                              kills(3) = kills(3) + 1;
                           end;
                        end;
                     end;
                  end;
                  if badPlacement
                     replacements = replacements + 1;
                     if replacements > maxReplacements
                        badPath = 1;
                        break;
                     end;
                  end;
               end;
               if badPath
                  break;
               end;
            end;

            if ~badPath && trialType(trial) == 2
               % move the first distracter such that it will reappear at the first target's
               % disappearance locations
               dirmod = fullCircle / 4 * Shuffle([1 -1]);
               for d = dirmod
                  direction(nTargets+1) = direction(1) + dirmod;
                  [finalTheta finalMagnitude] = addNoiseVector(fullCircle - direction(nTargets+1), ...
                                                               targetMagnitude*(blankDuration), 0, 0);
                  preBlankCoordinates(nTargets+1) = computeCoordinates(preBlankCoordinates(1,:), finalTheta, finalMagnitude);
                  % check if this pre-blank location is out-of-bounds
                  if OutOfBounds(preBlankCoordinates(nTargets+1)) == 0
                     badPath = 0;
                     break;
                  else
                     badPath = 1;
                  end;
               end;
               if badPath
                  kills(4) = kills(4) + 1;
               end;
            end;
            
            if ~badPath
               % move disks backwards until minDuration start frame
               frame = blankInterval(trial, 1) - 1;
               trajectory(:, :, frame) = preBlankCoordinates;
               oldCoordinates = trajectory(:, :, frame);
               reverse = mod(preBlankDirections + halfCircle - 1, fullCircle) + 1;
               startFrame = slackFrames;
               frame = frame - 1;
               while frame > 0
                  badPath = 1;

                  %% calculate new coordinates and then check for disks going out of bounds
                  for i = 1:3
                     [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0);
                     newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude);
                     status = OutOfBounds(newCoordinates, screenRect, edgeZone);
                     if status ~= 0
                        direction = BounceOffWall(direction, status);
                        badPath = 1;
                     else
                        badPath = 0;
                        break;
                     end;
                  end;
                  if badPath
                     kills(5) = kills(5) + 1; 
                     break;
                  end;

                  trajectory(:, :, frame) = newCoordinates;
                  oldCoordinates = newCoordinates;
                  % if we're at the start frame for the min trial duration, then start checking to
                  % see if the objects are a min distance apart.
                  if frame <= startFrame
                     badPath = 0;
                     for disk = 1:nDisks
                        x = newCoordinates(disk,:); y = newCoordinates(disk,:);
                        newCoordinates(disk, :) = [];
                        % break out of this loop as soon as we find two disks
                        % that are too close.
                        if any(sqrt((x - newCoordinates(:, 1)).^2 + (y - newCoordinates(:, 2)).^2) < bufferZone)
                           badPath = 1;
                           break;
                        end;
                     end;
                     if ~badPath
                        % we've found an appropriate starting frame:
                        trajectory = trajectory(:, :, frame:movieFrames(trial));
                        movieFrames(trial) = size(trajectory, 3);
                        blankInterval(trial, 1) = blankInterval(trial, 1) - frame + 1;
                        blankInterval(trial, 2) = blankInterval(trial, 2) - frame + 1;
                        break;
                     end;
                  end;
                  frame = frame - 1;
               end; % while frame > 0
               if badPath % then we weren't able to find a good starting frame
                  kills(6) = kills(6) + 1;
               end;
            end; % if ~badPath

            if ~badPath
               % now we know that trajectory(:, :, 1:(blankInterval-1)) is all properly spaced and now we
               % need to figure out the rest of the trajectory through to the end of the trial
               frame = blankInterval(trial, 2);
               trajectory(:, :, frame) = postBlankCoordinates;
               direction = postBlankDirections;
               oldCoordinates = trajectory(:, :, frame); 
               % move disks forward until the end of the trial:
               frame = frame + 1;
               while frame <= movieFrames(trial)
                  % adds a noise vector with random direction and noiseFactor*magnitude magnitude to the signal vector
                  %noiseDirection = 0 %Randi(fullCircle, [nDisks 1]); 
                  [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0); 
                  newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude); % recompute coordinates

                  % now check for disks going out of bounds
                  for disk = 1:nDisks
                     status = OutOfBounds(newCoordinates(disk,:), screenRect, edgeZone);
                     if status ~= 0
                        direction(disk) = BounceOffWall(direction(disk), status);
                     end;
                  end;

                  % re-compute newCoordinates to implement reversal:
                  [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0); 
                  newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude); % recompute coordinates

                  % now double-check to see if there is still a boundary violation
                  for disk = 1:nDisks
                     if OutOfBounds(newCoordinates(disk, :), screenRect, edgeZone)
                        badTurns = badTurns + 1;
                     end;
                  end;

                  trajectory(:, :, frame) = newCoordinates;
                  oldCoordinates = newCoordinates;
                  
                  frame = frame + 1;
               end; % while frame < movieFrames(trial)
               
               % now check final frame to make sure we have minimal separation between disks
               badPath = 0;
               newCoordinates = trajectory(:, :, movieFrames(trial));
               for disk = 1:nDisks
                  x = newCoordinates(disk,:); y = newCoordinates(disk,:);
                  newCoordinates(disk, :) = [];
                  % break out of this loop as soon as we find two disks
                  % that are too close.
                  if any(sqrt((x - newCoordinates(:, 1)).^2 + (y - newCoordinates(:, 2)).^2) < bufferZone)
                     badPath = 1;
                     kills(5) = kills(5) + 1;
                     break;
                  end;
               end;
            end; % if ~badPath

            if ~badPath
               % now we've got everything but the blank interval filled in:
               % set all blank interval coordinates to something off screen.
               trajectory(:, :, (blankInterval(trial,1)):(blankInterval(trial,2)-1)) = -50;
            end; % if ~badPath
            
            if badPath
               restarts = restarts + 1;
               if restarts > maxRestarts
                  break;
               end;
            end; 
         end; % while badPath
         
         if badPath
            break;
         end;
         paths{trial} = trajectory;
         starts{trial} = startCoordinates;
         clear trajectory;
      end; % for trial = 1:nTrials
           %	endTime = GetSecs;
           %timePassed = endTime - initTime
      kills
      badTurns
      if badPath
         ['ERROR: Too many restarted paths on trial ' num2str(trial)]
         break;
      else
         save (filename,  'paths', 'starts', 'movementRate', 'movementNoise', 'trialDuration', 'blankDuration', 'nTrials', 'nDisks', 'predictedMovieFrameDuration', 'movieFrames', 'shiftFactor', 'shiftAmount');
         fprintf(1, 'Finished %s.\n', filename);
      end;
      if badPath
         break;
      end;
   end; % for block = 1:nBlocks
   if badPath
      break;
   end;
end; % for sub = subjects

etime(clock, starttime)

function newCoordinates = computeCoordinates(oldCoordinates, theta, magnitude)

% global increment

%calculate the next position
   newCoordinates(:, 1) = magnitude.*sin(theta) + oldCoordinates(:, 1); %x coordinate of the next point
   newCoordinates(:, 2) = magnitude.*cos(theta) + oldCoordinates(:, 2); %y coordinate of the next point

function [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, noiseDirection, noiseMagnitude)

   degreesPerDirection = 360/24;

   direction = mod(direction - 1, 24) + 1; % eliminate negative directions
   theta = (direction*degreesPerDirection)*(pi/180); % direction to radians
   noiseTheta = (noiseDirection*degreesPerDirection)*(pi/180); % noise direction in radians

   %now convert to cartesian coordinates and add
   [signalX, signalY] = pol2cart(theta, magnitude);
   [noiseX, noiseY] = pol2cart(noiseTheta, noiseMagnitude);
   finalX = signalX + noiseX;
   finalY = signalY + noiseY;
   [finalTheta, finalMagnitude] = cart2pol(finalX, finalY);


function out = OutOfBounds (coordinates, edgeRect, border)
   [r c] = size(coordinates);
   if r == 1 && c == 2
      out = 0; %zeros(r, 1);
      if (coordinates(1) < (edgeRect(1) + border))
         % approaching left edge
         out = 1;
      elseif (coordinates(2) < (edgeRect(2) + border))
         % approaching ceiling
         out = 2;
      elseif (coordinates(1) > (edgeRect(3) - border))
         % approaching right edge
         out = 3;
      elseif (coordinates(2) > (edgeRect(4) - border))
         % approaching floor
         out = 4;
      end;
   else
      out = 5;
   end;

   

function newdir = BounceOffWall (olddir, wall)
% Bounce a disk off a wall. Direction is in the range 1:fullCircle,
% and wall is the output of the OutOfBounds function above.
   global fullCircle halfCircle;

   switch wall
    case 1
     % approaching left edge
     newdir = fullCircle - olddir;
    case 2
     % approaching ceiling
     newdir = halfCircle - olddir;
    case 3
     % approaching right edge
     newdir = fullCircle - olddir;
    case 4
     % approaching floor
     newdir = halfCircle - olddir;
   end;
