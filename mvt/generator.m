function pathsFile = stGenerator (sInitial)

% stGenerator:
% generates a set of trajectories for use with ShiftTrack experiments
% Authors: David Fencsik (based on file by Todd Horowitz)
%
% $Id: generator.m,v 1.16 2004/02/04 21:04:08 fencsik Exp $

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

subjects = 1; % e.g, 1:10 [ 2 7 11] 
%trialDuration = 4;
% Possible trial durations, in seconds
minTrialDuration = 5;
maxTrialDuration = 6;
nDisks = 10;
nTargets = 5;

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
              repmat(2, exptrials)
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
preBlankBorder = 80;

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
if blankWindow(1) >= blankWindow(2)
   'ERROR: Negative blank window requested.'
   return;
end;


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
preBlankRect = CenterRect(preBlankRect, screenRect);



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
      [trialType index] = Shuffle(trialTypes{block});
      movementRate = movementRates{block}(index);
      blankDuration = blankDurations{block}(index);
      nTrials = size(trialType, 1);

      blankStart = zeros(nTrials, 1);
      blankEnd = zeros(nTrials, 1);

      shiftFactor = ones(size(trialType));
      shiftAmount = ceil((shiftFactor-1.0) .* blankDuration);
      movieFrames = ones(nTrials, 1) * maxMovieFrames;
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
         blankEnd(trial) = blankStart(trial) + blankDuration(trial);

         trajectory = zeros(nDisks, 2, movieFrames(trial));

         % set up vectors of individual signal and noise magnitudes
         magnitude = ones(nDisks, 1) * targetMagnitude(trial); 
         % noisy movement is not being used here:
         % noiseMagnitude = zeros(nDisks, 1); 

         restarts = 0;
         badPath = 1;

         deathFlag = 999;
         while deathFlag > 0
            % quit if we've been going for too long:
            if sum(kills(1:(size(kills,2)-1))) > (200*nTrials)
               kills
               deathFlag
               'Too many kills.'
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
            end; % for d = 1:nDisks

            if deathFlag > 0
               continue;
            end;

            % finished finding a good set of starting points

            trajectory = zeros(nDisks, 2, movieFrames(trial));
            trajectory(:, :, 1) = startCoordinates;
            oldCoordinates = startCoordinates;

            direction = Randi(fullCircle, [nDisks 1]); % initial random directions in 24 degree increments
            magnitude = ones(nDisks, 1) * targetMagnitude(trial);
            noiseMagnitude = zeros(nDisks, 1);

            f = 2;
            while f <= movieFrames(trial)
            %for f = 2:movieFrames(trial)
               % move one frame:
               [x y direction err bounce] = MoveOneStep (oldCoordinates, direction, magnitude, screenRect, edgeZone, fullCircle);

               % note any errors:
               if any(err > 0)
                  deathFlag = 2;
                  kills(deathFlag) = kills(deathFlag) + 1;
                  break;
               end;
               
               newCoordinates = [x y];


               % make sure disks are a mindistance apart on last visible pre-gap frame
               if blankDuration(trial) > 0 & f == blankStart(trial) - 1
                  if any(InterDiskDistances(newCoordinates) < bufferZone)
                     deathFlag = 3;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     break;
                  %elseif any(OutOfBoundsBits(newCoordinates, screenRect, preBlankBorder) > 0)
                  %   deathFlag = 4;
                  %   kills(deathFlag) = kills(deathFlag) + 1;
                  %   break;
                  end;
               end;


               % make sure that all disks reappear at a mindistance from where the TARGETS disappeared.
               if blankDuration(trial) > 0 & f == blankEnd(trial) 
                  pre = trajectory(1:nTargets, :, blankStart(trial) - 1); 
                  post = newCoordinates; 
                  for d = 1:nTargets
                     post(1, :) = []; % get rid of the comparison disk
                     if any(DistancesFromDisk(pre(d,:), post) < bufferZone) %any(sqrt((pre(d,1) - post(:,1)).^2 + (pre(d,2) - post(:,2)).^2) < bufferZone)
                        % one or more disks reappear too close to a pre-blank target location
                        deathFlag = 5;
                        kills(deathFlag) = kills(deathFlag) + 1;
                        break;
                     end;
                  end;
                  if deathFlag > 0
                     break;
                  end;
               end;


               % if trialType(trial) = 2, then set the first distractor so it reappears at the same
               % location as the first target, and check to make sure it's start position
               % is far enough from all the other disks. And make sure it doesn't bounce
               % Reverse code:
               if blankDuration(trial) > 0 & f == blankEnd(trial) & trialType(trial) == 2
                  % try once for each target
                  deathFlag = 6;
                  for t = 1:nTargets
                     dirmod = fullCircle / 4 * Shuffle([1 -1]); % possible directions relative to target direction
                     for d = dirmod
                        dir = preBlankDirections(t) + d;
                        rev = mod(dir + halfCircle - 1, fullCircle) + 1;
                        testTraj = zeros(1, 2, f);
                        testTraj(1, :, f) = preBlankCoordinates(t, :); % get this targets preBlankCoordinates;
                        f2 = f-1;
                        while f2 > 0
                           [x y rev err bounce] = MoveOneStep (testTraj(1, :, f2+1), rev, magnitude(1), screenRect, edgeZone, fullCircle);
                           if any(err > 0) | any(bounce > 0)
                              break;
                           end;
                           testTraj(1, :, f2) = [x, y];
                           %if f2 == blankStart(trial) - 1
                           %   if any(DistancesFromDisk(testTraj(1, :, f2)) < buffer)
                           f2 = f2 - 1;
                        end;
                        if f2 == 0
                           % got to the end of the above loop
                           deathFlag = 0;
                           break;
                        end;
                     end; % for d = dirmod
                     if deathFlag == 0
                        break;
                     end;
                  end; % for t = 1:nTargets
                  if deathFlag > 0
                     % deathFlag will equal 6 or 0 here
                     kills(deathFlag) = kills(deathFlag) + 1;
                     break;
                  else
                     % set first distracter trajectory to testTraj
                     trajectory(nTargets+1, :, 1:f) = testTraj;
                     newCoordinates(nTargets+1, :) = trajectory(nTargets+1, :, f);
                     direction(nTargets+1) = dir;

                     % make whichever target was used above be the first target
                     if t > 1
                        tmp = trajectory(1, :, :);
                        trajectory(1, :, :) = trajectory(t, :, :);
                        trajectory(t, :, :) = tmp;
                        newCoordinates(1, :) = trajectory(1, :, f);
                        newCoordinates(t, :) = trajectory(t, :, f);
                        tmpdir = direction(1);
                        direction(1) = direction(t);
                        direction(t) = tmpdir;
                     end;
                  end;
               end; % if blankDuration(trial) > 0 & f == blankEnd(trial) & trialType(trial) == 2


               % set preBlankCoordinates
               if f == blankStart(trial) - 1
                  preBlankCoordinates = newCoordinates;
                  preBlankDirections = direction;
               end;

                  
               trajectory(:, :, f) = newCoordinates;
               % if f is between minMovieFrames and maxMovieFrames, then start checking to
               % see if the disks are far enough apart. As soon as they are, pick that as our
               % last frame
               if f >= minMovieFrames & f <= maxMovieFrames
                  if all(InterDiskDistances(newCoordinates) >= bufferZone)
                     % set movie frames
                     movieFrames(trial) = f;
                     trajectory = trajectory(:, :, 1:f);
                     break;
                  elseif f == maxMovieFrames
                     % we're at the end and still haven't found a good end-point
                     deathFlag = 7;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     break;
                  end;
               end;

               
               %set the old coordinates (for the next frame) equal to the coordinates used for the current frame 
               oldCoordinates = newCoordinates;
               f = f + 1;
            end; % while f <= movieFrames(trial)
         end; % while deathFlag > 0
         
         if blankDuration(trial) > 0
            trajectory(:, :, (blankStart(trial)):(blankEnd(trial)-1)) = -50;
         end;
         
         paths{trial} = trajectory;
         starts{trial} = startCoordinates;
         clear trajectory;
      end; % for trial = 1:nTrials
      kills
      save (filename, 'paths', 'starts', 'blankStart', 'movementRate', 'blankDuration', 'nTrials', 'nDisks', 'nTargets', 'predictedMovieFrameDuration', 'movieFrames', 'shiftFactor', 'shiftAmount');
      fprintf(1, 'Finished %s.\n', filename);
      % if badPath
      %    ['ERROR: Too many restarted paths on trial ' num2str(trial)]
      %    break;
      % else
      %    save (filename, 'nTrials', 'paths', 'starts', 'movementRate', 'movieFrames', 'blankDuration', 'trialType', 'nDisks', 'nTargets', 'predictedMovieFrameDuration');
      %    fprintf(1, 'Finished %s.\n', filename);
      % end;
      % if badPath
      %    break;
      % end;
   end; % for block = 1:nBlocks
   % if badPath
   %    break;
   % end;
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


function [distances] = DistancesFromDisk (disk, coordinates)
   [nrow ncol] = size(coordinates);
   if ncol ~= 2 | size(disk, 2) ~= 2
      'ERROR: DistancesFromDisk given non-coordinate input matrix'
      return;
   elseif size(disk, 1) > 1
      'ERROR: first argument to DistancesFromDisk must be coordinates of a single disk'
      return;
   else
      distances = sqrt((disk(1) - coordinates(:, 1)).^2 + (disk(2) - coordinates(:, 2)).^2);
   end;

%%% END OF DistancesFromDisk %%%


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


function [x, y, d, error, bounce] = MoveOneStep (coordinates, direction, magnitude, edgeRect, edgeZone, fullCircle)
% Moves all objects one step
   
   [nrow ncol] = size(coordinates);
   if nrow ~= size(direction,1) | nrow ~= size(magnitude, 1) | ncol ~= 2
      'ERROR: bad arguments passed to MoveOneStep'
      coordinates
      direction
      magnitude
      return;
   end;
   
   error = zeros(nrow, 1);
   bounce = zeros(nrow, 1);
   halfCircle = fullCircle / 2;

   [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0);
   newCoordinates = computeCoordinates(coordinates, finalTheta, finalMagnitude); % recompute coordinates

   % try to bounce out-of-bounds disks off a wall:
   status = OutOfBoundsBits(newCoordinates, edgeRect, edgeZone);
   if any(status ~= 0)
      bounce = (status ~= 0);
      direction = BounceOffWall(direction, status, fullCircle);
   end;

   % now recomputed movement with bounce:
   [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, 0, 0);
   newCoordinates = computeCoordinates(coordinates, finalTheta, finalMagnitude); % recompute coordinates

   % if any are still out of bounds, then a bounce was ineffective
   error = OutOfBoundsBits(newCoordinates, edgeRect, edgeZone);

   x = newCoordinates(:,1); 
   y = newCoordinates(:,2); 
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
