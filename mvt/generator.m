function pathsFile = generator (sInitial)

%%% generator:
%%% generates a set of trajectories for use with Multiple Object Tracking experiments
%%% Authors: David Fencsik (based on file by Todd Horowitz)
%%%
%%% Version: $Revision: 1.32 $ $Date: 2004/08/16 15:49:28 $ (UTC)

starttime = clock;
debug = 0;
stopAfterNKills = 100;

subjects = 1; % e.g, 1:10 [ 2 7 11] 
%trialDuration = 4;
% Possible trial durations, in seconds
minTrialDuration = 6;
maxTrialDuration = 6;
nDisks = 8;
%nTargets = 2;

% block-level variables
prefix = {%'trainA'; 
          'prac2s'; 'exp2s'; 
          'prac2m'; 'exp2m'; 
          'prac4s'; 'exp4s'; 
          'prac4m'; 'exp4m';
          'trainB';
         };
prefix = {'pracA'; 'pracB'; 'pracC'; 'expA'; 'expB'; 'expC';}; % for testing
practrials = [2 1];
exptrials = [10 1];
mvttrials = [30 1];
testtrials = [1 1];
trialTypes = {%repmat(1, mvttrials);
              repmat(0, practrials); repmat(0, exptrials);
              repmat(1, practrials); repmat(1, exptrials);
              repmat(0, practrials); repmat(0, exptrials);
              repmat(1, practrials); repmat(1, exptrials);
              repmat(1, mvttrials);
             };
movementRates = {%repmat(9, mvttrials);
                 repmat(9, practrials); repmat(9, exptrials);
                 repmat(9, practrials); repmat(9, exptrials);
                 repmat(9, practrials); repmat(9, exptrials);
                 repmat(9, practrials); repmat(9, exptrials);
                 repmat(9, mvttrials);
                };
blankDurations = {%repmat(0, mvttrials);
                  repmat(23, practrials); repmat(23, exptrials);
                  repmat(23, practrials); repmat(23, exptrials);
                  repmat(23, practrials); repmat(23, exptrials);
                  repmat(23, practrials); repmat(23, exptrials);
                  repmat(23, mvttrials);
                 }; % 23 = 307 ms, 30 = 400 ms, 38 = 507 ms, 45 = 600 ms
trialTypes = {repmat(-1, practrials); repmat(0, practrials); repmat(1, practrials); 
              repmat(-1, exptrials); repmat(0, exptrials); repmat(1, exptrials);};
movementRates = {repmat(9, practrials); repmat(9, practrials); repmat(9, practrials); 
                 repmat(9, exptrials); repmat(9, exptrials); repmat(9, exptrials);};
blankDurations = {repmat(23, practrials); repmat(23, practrials); repmat(23, practrials); 
                 repmat(23, exptrials); repmat(23, exptrials); repmat(23, exptrials);}; % 23 = 307 ms, 30 = 400 ms, 38 = 507 ms, 45 = 600 ms

switchedDisk = 0;
nBlocks = size(trialTypes,1);
if nBlocks ~= size(movementRates,1) | nBlocks ~= size(blankDurations,1)
   'ERROR: block variables are not the same size'
   return;
end;

for block = 1:nBlocks
   if size(trialTypes{block}, 1) ~= size(movementRates{block}, 1) | ...
          size(trialTypes{block}, 1) ~= size(blankDurations{block}, 1) | ...
          size(trialTypes{block}, 2) ~= size(movementRates{block}, 2) | ...
          size(trialTypes{block}, 2) ~= size(blankDurations{block}, 2)
      ['ERROR: Block ' num2str(block) ' contains matrices of different size']
      return;
   end;
end;


bufferZone = 50;
preBlankRect = [0 0 1024-200 768-100];
preBlankBorder = 50;

clear screen;


% major variables

% timing variables
predictedMovieFrameDuration = 13.33;
maxMovieFrames = ceil(maxTrialDuration*1000/predictedMovieFrameDuration);
minMovieFrames = ceil(minTrialDuration*1000/predictedMovieFrameDuration);
%%% slackDuration = maxTrialDuration - minTrialDuration;
%%% slackFrames = ceil(slackDuration*1000/predictedMovieFrameDuration);
blankWindowStart = round(2000/predictedMovieFrameDuration);
blankWindowEnd   = minMovieFrames - round(1000/predictedMovieFrameDuration);
%%% if blankWindow(1) >= blankWindow(2)
%%%    'ERROR: Negative blank window requested.'
%%%    return;
%%% end;


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
%%cxy = newGrids(xdim, ydim, windowX, windowY, screenRect);


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

      % determine trial length (in frames)
      movieFrames = minMovieFrames + Randi(maxMovieFrames - (minMovieFrames-1), [nTrials,1]) - 1;
      %% blank interval is picked separately for each disk in ShiftTrack5
      % blankStart = movieFrames - blankDuration;
      % blankEnd = movieFrames;

      % movement vector length
      targetMagnitude = (movementRate*30)*(predictedMovieFrameDuration)/1000;

      % use kills to keep track of where problems are occuring
      kills = zeros(1,10);
      badTurns = 0;

      for trial = 1:nTrials
         % Procedure for ShiftTrack5, which implements the basic shifttrack
         % manipulations with asynchronous disappearance.
         %
         % 1. Pick random blank intervals, separately for each disk
         % 2. Pick random starting locations
         % 3. Once we get to X frames before each disk's gap, 
         %    start making sure there's no bounces.
         % 4. As we get to the end of each disk's gap, reset it's 
         %    reappearance position.
         % 5. When we get to the last frame, make sure stimuli are 
         %    a min distance apart.
         % 6. Reset each disk's position during its gap to an off-screen
         %    location.

         if debug
            [movieFrames(trial),  blankStart(trial), blankEnd(trial)]
         end;
         
         % Pick random blank intervals for each disk
         blankStart = Randi(blankWindowEnd - blankWindowStart + 1, [nDisks, 1]) + ...
             blankWindowStart - 1;
         blankEnd = blankStart + blankDuration(trial);
         
         trajectory = zeros(nDisks, 2, movieFrames(trial));

         % set up vectors of individual signal and noise magnitudes
         magnitude = ones(nDisks, 1) * targetMagnitude(trial); 
         % noisy movement is not being used here:
         % noiseMagnitude = zeros(nDisks, 1); 

         restarts = 0;
         badPath = 1;
         
         redoLocationsCounter = 0;

         deathFlag = 999;
         while deathFlag > 0
            % quit if we've been going for too long:
            if stopAfterNKills > 0 & sum(kills(1:(size(kills,2)-1))) > (stopAfterNKills*nTrials)
               kills = [1:(size(kills, 2)); kills]
               fprintf(1, 'Too many kills.\n');
               fprintf(1, 'Last kill = %d\n', deathFlag);
               return;
            end;

            % while loop waits until a viable trajectory is generated
            deathFlag = 0;

            if redoLocationsCounter > 0
               % keep the same locations and pick new directions
               redoLocationsCounter = redoLocationsCounter - 1;
            else
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
               if ~badPlacement
                  if debug
                     fprintf(1, 'Picked new locations. ');
                  end;
                  redoLocationsCounter = 0;
               end;
            end;

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
               % move one frame:
               [x y direction err bounce] = MoveOneStep (oldCoordinates, direction, magnitude, screenRect, edgeZone, fullCircle);

               % note any errors:
               if any(err > 0)
                  deathFlag = 2;
                  kills(deathFlag) = kills(deathFlag) + 1;
                  if debug
                     fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f);
                  end;
                  break;
               end;
               
               newCoordinates = [x y];
               trajectory(:, :, f) = newCoordinates;


               % make sure disks don't bounce anywhere between the -1 and 1 positions
               % (but only check those in their blank interval
               if blankDuration(trial) > 0 & any(f > blankStart - blankDuration(trial) & f <= blankEnd)
                  disks = (f > blankStart - blankDuration(trial) & f <= blankEnd);
                  if any(bounce(disks) > 0)
                     deathFlag = 3;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     if debug
                        fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f);
                     end;
                     break;
                  end;
               end;


               %% % make sure disks are a mindistance apart on last visible pre-gap frame
               %% if blankDuration(trial) > 0 & f == blankStart(trial) - 1
               %%    if any(InterDiskDistances(newCoordinates) < bufferZone)
               %%       deathFlag = 4;
               %%       kills(deathFlag) = kills(deathFlag) + 1;
               %%       if debug
               %%          fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f);
               %%       end;
               %%       break;
               %%    end;
               %% end;


               %% % set preBlankCoordinates
               %% if blankDuration(trial) > 0 & f == blankStart(disk) - 1
               %%    preBlankCoordinates = newCoordinates;
               %%    preBlankDirections = direction;
               %% end;


               % At the end of each disk's blank interval, adjust its reappearance
               % position according to the current trialType
               if blankDuration(trial) > 0 & any(f == blankEnd)
                  disks = (f == blankEnd);
                  if trialType(trial) == -1
                     % move to position -1
                     trajectory(disks, :, f) = trajectory(disks, :, f - (2 * blankDuration(trial)) - 1);
                  elseif trialType(trial) == 0
                     % move to position 0
                     trajectory(disks, :, f) = trajectory(disks, :, f - blankDuration(trial) - 1);
                  elseif trialType(trial) == 1
                     % move to position 1
                     % nothing to do
                  else
                     ['ERROR: unknown trial type ', num2str(trialType), ' requested.']
                     return;
                  end;
               end;


               % if f is the final frame, then check that everything is far enough apart
               if f == movieFrames(trial)
                  if any(InterDiskDistances(trajectory(:, :, f)) < bufferZone)
                     % end point is no good
                     deathFlag = 5;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     if debug
                        fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f);
                     end;
                     break;
                  end;
               end;
               

               %set the old coordinates (for the next frame) equal to the coordinates used for the current frame 
               oldCoordinates = trajectory(:, :, f);
               f = f + 1;
            end; % while f <= movieFrames(trial)
         end; % while deathFlag > 0
         
         if blankDuration(trial) > 0
            for d = 1:nDisks
               trajectory(d, :, (blankStart(d)):(blankEnd(d)-1)) = -100;
            end;
         end;
         
         paths{trial} = trajectory;
         starts{trial} = startCoordinates;
         clear trajectory;
      end; % for trial = 1:nTrials

      [1:(size(kills,2)); kills]
      save (filename, 'paths', 'starts', 'blankStart', 'movementRate', 'blankDuration', 'nTrials', 'trialType', 'nDisks', 'predictedMovieFrameDuration', 'movieFrames');
      fprintf(1, 'Finished %s.\n', filename);
   
   end; % for block = 1:nBlocks
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
