function pathsFile = stGenerator (sInitial)

% stGenerator:
% generates a set of trajectories for use with ShiftTrack experiments
% Authors: David Fencsik (based on file by Todd Horowitz)
%
% $Id: generator.m,v 1.8 2004/01/09 17:02:00 fencsik Exp $

% Modified by David Fencsik
% started  9/29/2003
% based on tGenerator6c of  9/15/2003
% version of  01/06/2004

% Fix within-block design so certain variables can be implemented within-block or
% between-block. Each of these variables has a WB version that, if non-empty, is
% manipulated within-block.

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

reappearanceOffset = [1.0];
repetitions = 50; % one value, or one value per trial type
practiceRepetitions = 10; 
prefix = {'test'} %{'train';'a';'b';'c'}; 
                  % if withinBlock = 0, then one value per trial type, otherwise any
                  % value(s).
practicePrefix = 'p_'; %  

movementRate = 8;
movementNoise = 0;
predictedMovieFrameDuration = 13.33;
blankDurations = [0; 23; 23; 23];

bufferZone = 50;
preBlankRect = [0 0 1024 768];

nTrialTypes = size(reappearanceOffset,1);


clear screen;

%  major variables

% timing variables
maxMovieFrames = ceil(maxTrialDuration*1000/predictedMovieFrameDuration);
minMovieFrames = ceil(minTrialDuration*1000/predictedMovieFrameDuration);
slackDuration = maxTrialDuration - minTrialDuration
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
fullCircle = 24;
halfCircle = 12;
% increment = (rate*30)*(frameDuration);
targetMagnitude = (movementRate*30)*(predictedMovieFrameDuration)/1000;
targetNoiseMagnitude = (movementNoise*30)*(predictedMovieFrameDuration)/1000;
%distractorMagnitude = (distractorRate*30)*(predictedMovieFrameDuration);
%distractorNoiseMagnitude = (distractorNoise*30)*(predictedMovieFrameDuration);
degreesPerDirection = 360/24;

tau = clock;
rand('state',sum(100*tau));
state=rand('state');


% ensure that pre-gap rectangle is not too big and is centered within the screenRect
preBlankRect(1) = preBlankRect(1) + edgeZone;
preBlankRect(2) = preBlankRect(2) + edgeZone;
preBlankRect(3) = preBlankRect(3) - edgeZone;
preBlankRect(4) = preBlankRect(4) - edgeZone;
if preBlankRect(3) - preBlankRect(1) > screenX
   preBlankRect(1) = 0;
   preBlankRect(3) = screenX;
end;
if preBlankRect(4) - preBlankRect(2) > screenY
   preBlankRect(2) = 0;
   preBlankRect(4) = screenY;
end;
preBlankRect = CenterRect(preBlankRect, screenRect);



% startCoordinates = zeros(nBalls, 2);
xdim = 5; ydim = 6;% a 5 by 6 grid of positions
windowX = 100;windowY = 100;% each cell is 125 by 125 pixels
cxy = newGrids(xdim, ydim, windowX, windowY, screenRect);

for sub = subjects
   %	initTime = GetSecs;
   kills = [0; 0; 0];
   
%    if withinBlock == 0
%       cycles = nTrialTypes;
%    else
%       fprintf(2, 'ERROR: withinBlock != 0 not yet implemented\n');
%       return
%    end;
   
   for i = 1:cycles
      for prac = [0 1]
         % if (withinBlock == 0)
         %    filename = [prefix{i} num2str(sub)];
         %    if prac == 0
         %       shiftFactor = repmat(trialTypes(i), repetitions, 1);
         %    else
         %       shiftFactor = repmat(trialTypes(i), practiceRepetitions, 1);
         %       filename = [practicePrefix filename];
         %    end
         %    if (size(blankDurations,1) > 1)
         %       blankDuration = blankDurations(i);
         %    else
         %       blankDuration = blankDurations(1);
         %    end;
         %    shiftAmount = ceil((shiftFactor-1.0) * blankDuration);
         %    nTrials = size(shiftFactor, 1);
         %    movieFrames = ones(nTrials, 1) * estMovieFrames + shiftAmount;
         % end; % if (withinBlock == 0)
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

            ttype = trialType(trial)
            
            % Determine the first invisible blank-interval frame: The gap can occur at 
            % least 2 seconds after the start of tracking but no later than 1 second 
            % before the end of tracking:
            blankWindow(1) = round(2000/predictedMovieFrameDuration);
            blankWindow(2) = minMovieFrames - round(1000/predictedMovieFrameDuration);
            blankInterval(trial,1) = randi(blankWindow(2) - blankWindow(1)) + blankWindow(1)
            blankInterval(trial,2) = blankInterval(trial,1) + blankDuration
            
            % compute trajectories
            trajectory = zeros(nDisks, 2, maxMovieFrames);
            deathFlag = 999;
            while deathFlag > 0
               % place each disk, one at a time, give it a trajectory, and make sure it
               % is both a minimum distance from other disks and will not reappear too
               % close to any target disk after the blank interval.
               preBlankCoordinates = zeros(nDisks,2);
               direction = Randi(fullCircle, [nDisks 1]); % initial random directions in 24 degree increments
               for disk = 1:nDisks
                  badPlacement = 1;
                  while badPlacement
                     % pick a location in the rectangle
                     x = Randi(preBlankRect(3) - preBlankRect(1));
                     y = Randi(preBlankRect(4) - preBlankRect(2));
                     preBlankCoordinates(disk,:) = [x y];
                     badPlacement = 0;
                     if disk > 1
                        % check to make sure that pre-blank coordinates are far enough
                        % from any other disks
                        for disk2 = 1:(disk-1)
                        end;
                        % check that reappearance location of this disk is far enough
                        % from the reappearance of any targets.
                        for disk2 = 1:nTargets
                        end;
                     end;
                  end;
               end;
               
              
               % 			if kills > 1000;
               % 				clear screen;
               % 				kills
               % 				return;
               % 			end;

               % create a random permutation of the 35 possible locations
               randselect = randperm(xdim*ydim);

               %starting positions for each ball are selected from the randomset
               startCoordinates = cxy(randselect(1:nDisks), :);
               oldCoordinates = startCoordinates;
               newCoordinates = startCoordinates;
               direction = Randi(24, [nDisks 1]); % initial random directions in 24 degree increments
               theta = (direction*30)*(pi/180); % initial direction in radians
               % set up vectors of individual signal and noise magnitudes
               magnitude = ones(nDisks, 1) * targetMagnitude; 
               noiseMagnitude = ones(nDisks, 1) * targetNoiseMagnitude; 
               %magnitude(1:tracknumber) = targetMagnitude;
               %noiseMagnitude(1:tracknumber) = targetNoiseMagnitude;


               deathFlag = 0;
               for f = 1:movieFrames(trial)
                  % while loop waits until a viable trajectory is generated
                  count = 0;
                  count = count+1;
                  noiseDirection = Randi(24, [nDisks 1]); % random directions in 24 degree increments

                  % adds a noise vector with random direction and noiseFactor*magnitude magnitude to the signal vector
                  [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, noiseDirection, noiseMagnitude); 
                  newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude); % recompute coordinates

                  % now prevent distractors from going out of bounds

                  for a = 1:nDisks
                     if (newCoordinates(a, 2) > (screenY - edgeZone))
                        % approaching floor
                        direction(a) = halfCircle - direction(a);
                     elseif (newCoordinates(a, 2) < edgeZone)
                        % approaching ceiling
                        direction(a) = halfCircle - direction(a);
                     elseif (newCoordinates(a, 1) > (screenX - edgeZone))
                        % 						if (newCoordinates(a, 1) > (screenX - edgeZone))
                        % approaching right edge
                        direction(a) = fullCircle - direction(a);
                     elseif (newCoordinates(a, 1) < edgeZone)
                        % approaching left edge
                        direction(a) = fullCircle - direction(a);
                     else
                        % do nothing

                     end
                  end

                  % adds a noise vector with random direction and noiseFactor*magnitude magnitude to the signal vector
                  [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, noiseDirection, noiseMagnitude); 
                  newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude); % recompute coordinates

                  % now double-check to see if there is occlusion or boundary violation

                  for a = 1:nDisks
                     if (newCoordinates(a, 2) > (screenY - imageY))|(newCoordinates(a, 2) < imageY)
                        % floor or ceiling
                        deathFlag = 1;
                        kills = kills +1;
                        % ['vertical boundary violation with ball ' num2str(a)]
                        % ['  coordinates (' num2str(newCoordinates(a, 1)) ',' num2str(newCoordinates(a, 2)) ')']
                        % ['  frame ' num2str(f)']
                        % clear screen;
                        % return;
                     elseif (newCoordinates(a, 1) > (screenX - imageX))|(newCoordinates(a, 1) < imageX)
                        deathFlag = 2;
                        kills = kills +1;
                        % ['horizontal boundary violation with ball ' num2str(a)]
                        % ['  coordinates (' num2str(newCoordinates(a, 1)) ',' num2str(newCoordinates(a, 2)) ')']
                        % clear screen;
                        % return;
                     end
                  end

                  % now prevent balls from occluding one another on final frame
                  if f == movieFrames(trial)
                     for b = 1:nDisks
                        for c = (b+1):nDisks
                           cbd = newCoordinates(b, :) - newCoordinates(c, :);
                           interElementDistance = sqrt(sum(cbd.^2));
                           if interElementDistance < bufferZone
                              deathFlag = 3;
                              kills = kills +1;
                              % ['occlusion between balls ' num2str(b) ' and ' num2str(c) ' on final frame ']
                              % clear screen;
                              % return;
                           end
                        end
                     end
                  end

                  trajectory(:, :, f) = newCoordinates;
                  %set the old coordinates (for the next frame) equal to the coordinates used for the current frame 
                  oldCoordinates = newCoordinates;
               end; % for f = 1:movieFrames(trial)
            end; % while deathFlag > 0

            paths{trial} = trajectory;
            starts{trial} = startCoordinates;
            clear trajectory;
         end; % for trial = 1:nTrials
         %	endTime = GetSecs;
         %timePassed = endTime - initTime
         kills
         save (filename,  'paths', 'starts', 'movementRate', 'movementNoise', 'trialDuration', 'blankDuration', 'nTrials', 'nDisks', 'predictedMovieFrameDuration', 'movieFrames', 'shiftFactor', 'shiftAmount');
         fprintf(1, 'Finished %s.\n', filename);
      end; % for prac = [0 1]
   end; % for i = 1:cycles
end; % for sub = subjects



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
