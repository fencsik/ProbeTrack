function pathsFile = stGenerator (sInitial)

% stGenerator:
% generates a set of trajectories for use with ShiftTrack experiments
% Authors: David Fencsik (based on file by Todd Horowitz)
%
% $Id: generator.m,v 1.14 2004/02/02 23:03:48 fencsik Exp $

% Modified by David Fencsik
% started  9/29/2003
% based on tGenerator6c of  9/15/2003
% version of  01/06/2004

starttime = clock;
   
% Scrap this:
% withinBlock = 0; % 1 = within-block design, each prefix gets R repetitions of each trial
%                  % type (not implemented); 
%                  % 0 = between-block design, each prefix gets R
%                  % repetitions of one trial type

subjects = 1:12; % e.g, 1:10 [ 2 7 11] 
%trialDuration = 4;
% Possible trial durations, in seconds
minTrialDuration = 5;
maxTrialDuration = 6;
nDisks = 10;
nTargets = 2;

% block-level variables
prefix = {'mvt_'; 'pracA_'; 'pracB_'; 'pracC_'; 'pracD_'; 'exp_'};
prefix = {'test'}; % for testing
practrials = [10 1];
exptrials = [1 1];
mvttrials = [30 1];
trialTypes = {%repmat(1, mvttrials);
              %repmat(1, practrials);
              %repmat(1, practrials);
              %repmat(1, practrials);
              %repmat(1, practrials);
              repmat(1, exptrials)
             };
movementRates = {%repmat(9, mvttrials);
                 %repmat(9, practrials);
                 %repmat(9, practrials);
                 %repmat(9, practrials);
                 %repmat(9, practrials);
                 repmat(9, exptrials)
                };
blankDurations = {%repmat(0, mvttrials);
                  %repmat(23, practrials);
                  %repmat(23, practrials);
                  %repmat(23, practrials);
                  %repmat(23, practrials);
                  repmat(23, exptrials);
                 }; % 23 = 307 ms, 30 = 400 ms, 38 = 507 ms, 45 = 600 ms

nBlocks = size(trialTypes,1);
if nBlocks ~= size(movementRates,1) || nBlocks ~= size(blankDurations,1)
   'ERROR: block variables are not the same size'
   return;
end;

for block = 1:nBlocks
   if size(trialTypes{block}, 1) ~= size(movementRates{block}, 1) || ...
          size(trialTypes{block}, 1) ~= size(blankDurations{block}, 1) || ...
          size(trialTypes{block}, 2) ~= size(movementRates{block}, 2) || ...
          size(trialTypes{block}, 2) ~= size(blankDurations{block}, 2)
      ['ERROR: Block ' num2str(block) ' contains matrices of different size']
      return;
   end;
end;


bufferZone = 50;
preBlankRect = [0 0 1024-200 768-100];

clear screen;


% major variables

% timing variables
predictedMovieFrameDuration = 13.33;
maxMovieFrames = ceil(maxTrialDuration*1000/predictedMovieFrameDuration);
minMovieFrames = ceil(minTrialDuration*1000/predictedMovieFrameDuration);
slackDuration = maxTrialDuration - minTrialDuration;
slackFrames = ceil(slackDuration*1000/predictedMovieFrameDuration);
blankWindow(1) = round(2000/predictedMovieFrameDuration);
blankWindow(2) = minMovieFrames - round(1000/predictedMovieFrameDuration);


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

% center pre-gap rectangle within screenRect
preBlankRect = CenterRect(preBlankRect, screenRect)



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
      
      filename = [prefix{block} num2str(sub)];
      [conditions index] = Shuffle(trialTypes{block});
      movementRate = movementRates{block}(index);
      blankDuration = blankDurations{block}(index);
      nTrials = size(conditions, 1);

      blankStart = zeros(nTrails);

      shiftFactor = ttypes;
      shiftAmount = ceil((shiftFactor-1.0) .* blankDuration);
      movieFrames = ones(1, nTrials) * maxMovieFrames;
      % blankStart should be picked randomly for each trial:
      %blankStart = movieFrames - blankDuration;
      targetMagnitude = (movementRate*30)*(predictedMovieFrameDuration)/1000;

      % use kills to keep track of where problems are occuring
      kills = zeros(1,10);
      badTurns = 0;

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
         blankStart(trial) = Randi(blankWindow(2) - blankWindow(1)) + blankWindow(1);

         % compute trajectories
         trajectory = zeros(nDisks, 2, movieFrames(trial));
         % set up vectors of individual signal and noise magnitudes
         magnitude = ones(nDisks, 1) * targetMagnitude; 
         noiseMagnitude = zeros(nDisks, 1); 

         restarts = 0;
         badPath = 1;

         % compute trajectories
         deathFlag = 999;

         while deathFlag > 0
            % quit if we've been going for too long:
            if sum(kills(1:(size(kills,2)-1))) > (20*nTrials)
               kills
               return;
            end;

            % while loop waits until a viable trajectory is generated
            deathFlag = 0;

            % find starting coordinates by brute force: repeatedly select random points
            % for each disk until it is far enough from all the others:
            startCoordinates = zeros(nDisks, 2);
            for d = 1:nDisks
               replacements = 0;
               badPlacement = 1;

               while badPlacement
                  startCoordinates(d, 1) = Randi(screenX - (2 * edgeZone)) + edgeZone;
                  startCoordinates(d, 2) = Randi(screenY - (2 * edgeZone)) + edgeZone;
                  if d > 1
                     if all(InterDiskDistances(startCoordinates(1:d,:)) >= bufferZone)
                        badPlacement = 0;
                     else
                        replacements = replacements + 1;
                        if replacements > 1000
                           break;
                        end;
                     end;
                  else
                     badPlacement = 0;
                  end;
               end;
               if badPlacement
                  deathFlag = 1;
                  kills(deathFlag) = kills(deathFlag) + 1;
                  break;
               else
                  i = size(kills,2);
                  kills(i) = kills(i) + replacements;
               end;
            end;

            if badPlacement
               continue;
            end;

            % finished finding a good set of starting points

            trajectory = zeros(nDisks, 2, movieFrames(trial));
            trajectory(:, :, 1) = startCoordinates;
            oldCoordinates = startCoordinates;

            direction = Randi(fullCircle, [nDisks 1]); % initial random directions in 24 degree increments
            magnitude = ones(nDisks, 1) * targetMagnitude(trial);
            noiseMagnitude = zeros(nDisks, 1);

            for f = 2:movieFrames(trial)
               % move one frame:
               %function [x y d error] = MoveOneStep (coordinates, direction, magnitude, edgeRect, edgeZone, fullCircle)
               [x y direction err] = MoveOneStep (oldCoordinates, direction, magnitude, screenRect, edgeZone, fullCircle);

               % note any errors:
               if any(err > 0)
                  deathFlag = 2;
                  kills(deathFlag) = kills(deathFlag) + 1;
                  break;
               end;
               
               newCoordinates = [x y];

               % make sure disks are a mindistance apart on last visible pre-gap frame
               if blankDuration(trial) > 0 && f == blankStart(trial) - 1
                  if any(InterDiskDistances(newCoordinates) < bufferZone)
                     deathFlag = 3;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     break;
                  end;
               end;
               
               % make sure disks are within a limited rectangle on the last visible pre-gap frame
               if (blankDuration(trial) > 0 && f == blankStart(trial) - 1
                  if any(OutOfBoundsBits(newCoordinates, screenRect, preGapBorder) > 0)
                     deathFlag = 4;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     break;
                  end;
               end;

               % make sure the disks are a mindistance from all of the targets in the first visible
               % post-gap frame
               ADD CODE HERE

               % if condition = 2, then set the first distractor so it reappears at the same location
               % as as the first target
               ADD CODE HERE

               % now prevent balls from occluding one another on final frame
               if f == movieFrames(trial)
                  if any(InterDiskDistances(newCoordinates) < bufferZone)
                     deathFlag = 5;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     break;
                  end;
               end;
               
               trajectory(:, :, f) = newCoordinates;
               %set the old coordinates (for the next frame) equal to the coordinates used for the current frame 
               oldCoordinates = newCoordinates;
            end; % for f = 1:movieFrames(trial)
         end; % while deathFlag > 0
      
      end; % for trial = 1:nTrials

      kills

      if badPath
         ['ERROR: Too many restarted paths on trial ' num2str(trial)]
         break;
      else
         save (filename, 'nTrials', 'paths', 'starts', 'movementRate', 'movieFrames', 'blankDuration', 'trialType', 'nDisks', 'nTargets', 'predictedMovieFrameDuration');
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

%%% END OF computeCoordinates %%%


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

%%% END OF addNoiseVector %%%


function [distances] = InterDiskDistances (coordinates)
   [nrow ncol] = size(coordinates);
   if ncol ~= 2
      'ERROR: InterDiskDistances given non-coordinate input matrix'
      return;
   elseif nrow == 1
      'ERROR: InterDiskDistances given only one pair of coordinates'
      return;
   else
      distances = [];
      for d = 1:nrow
         x = coordinates(1, 1); y = coordinates(1, 2);
         coordinates(1, :) = [];
         distances = [distances; ...
                      sqrt((x - coordinates(:, 1)).^2 + (y - coordinates(:, 2)).^2)];
      end;
   end;

%%% END OF InterDiskDistances %%%


function out = OutOfBounds (coordinates, edgeRect, border)
% Returns 1 if any coordinates are outside of the edgeRect
% minus the border, 0 otherwise.
   [nrow ncol] = size(coordinates);
   out = 0;
   if ncol == 2
      if any(coordinates(:,1) < (edgeRect(1) + border)) | ...
               any(coordinates(:,2) < (edgeRect(2) + border)) | ...
               any(coordinates(:,1) > (edgeRect(3) - border)) | ...
               any(coordinates(:,2) > (edgeRect(4) - border))
         out = 1;
      end;
   elseif ncol < 2
      'ERROR: OutOfBounds given input with less than two columns'
      return;
   else
      'ERROR: OutOfBounds given input with more than two columns'
      return;
   end;

%%% END OF OutOfBounds %%%


function [x y d error] = MoveOneStep (coordinates, direction, magnitude, edgeRect, edgeZone, fullCircle)
% Moves all objects one step
   
   [nrow ncol] = size(coordinates);
   if nrow ~= size(coordinates,1) | nrow ~= size(magnitude, 1) | ncol ~= 2
      'ERROR: bad arguments passed to MoveOneStep'
      return;
   end;
   
   error = zeros(nrow, 1);
   halfCircle = fullCircle / 2;

   [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0);
   newCoordinates = computeCoordinates(coordinates, finalTheta, finalMagnitude); % recompute coordinates

   % try to bounce out-of-bounds disks off a wall:
   status = OutOfBoundsBits(newCoordinates, edgeRect, edgeZone);
   if any(status ~= 0)
      direction = BounceOffWall(direction, status, fullCircle);
   end;

   % now recomputed movement with bounce:
   [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0);
   newCoordinates = computeCoordinates(coordinates, finalTheta, finalMagnitude); % recompute coordinates

   % if any are still out of bounds, then a bounce was ineffective
   error = OutOfBoundsBits(newCoordinates, edgeRect, edgeZone);

   x = newCoordinates(:,1); 
   y = newCoordinates; 
   d = direction;

%%% END OF MoveOneStep %%%



function out = OutOfBoundsBits (coordinates, edgeRect, border)
% Sets bits of out array, such that 
%    bit 1 (1): approaching left edge
%    bit 2 (2): approaching ceiling
%    bit 3 (4): approaching right edge
%    bit 4 (8): approaching floor
%           0 : otherwise
   [nrow ncol] = size(coordinates);
   out = zeros(nrow, 1);
   if ncol == 2
      left    = (coordinates(:,1) < (edgeRect(1) + border)); % approaching left edge
      ceiling = (coordinates(:,2) < (edgeRect(2) + border)); % approaching ceiling
      right   = (coordinates(:,1) > (edgeRect(3) - border)); % approaching right edge
      floor   = (coordinates(:,2) > (edgeRect(4) - border)); % approaching floor

      out(left)    = out(left) + 1;
      out(ceiling) = out(ceiling) + 2;
      out(right)   = out(right) + 4;
      out(floor)   = out(floor) + 8;
   elseif ncol < 2
      'ERROR: OutOfBoundsBits given input with less than two columns'
      return;
   else
      'ERROR: OutOfBoundsBits given input with more than two columns'
      return;
   end;

%%% END OF OutOfBoundsBits %%%
   

function newdir = BounceOffWall (olddir, wall, fullCircle)
% Bounce a disk off a wall. Direction is in the range 1:fullCircle,
% and wall is the output of the OutOfBoundsBits function above.
   halfCircle = fullCircle/2;

   if size(olddir,1) == size(wall,1)
      newdir  = olddir;
      left    = (bitand(wall,1) > 0);
      ceiling = (bitand(wall,2) > 0);
      right   = (bitand(wall,4) > 0);
      floor   = (bitand(wall,8) > 0);

      newdir(left)    = fullCircle - newdir(left);
      newdir(ceiling) = halfCircle - newdir(ceiling);
      newdir(right)   = fullCircle - newdir(right);
      newdir(floor)   = halfCircle - newdir(floor);
   else
      'ERROR: BounceOfWall given mismatched input'
      return;
   end;

%%% END OF BounceOffWall %%%
