function pathsFile = stGenerator (sInitial)

% stGenerator:
% generates a set of trajectories for use with ShiftTrack experiments
% Authors: David Fencsik (based on file by Todd Horowitz)
%
% $Id: generator.m,v 1.4 2004/01/06 16:28:56 fencsik Exp $

% Modified by David Fencsik
% started  9/29/2003
% based on tGenerator6c of  9/15/2003
% version of  11/25/2003

withinBlock = 0; % 1 = within-block design, each prefix gets R repetitions of each trial
                 % type (not implemented); 
                 % 0 = between-block design, each prefix gets R
                 % repetitions of one trial type
movingCue = 1; % 0 = objects are cued in first frame and not handled by this file.  
               % 1 = objects begin moving at cuingRate with cuingNoise for
               % cuingDuration, then switch to standard movement rates
subjects = 1; % e.g, 1:10 [ 2 7 11] 
trialDuration = 5;
nDisks = 10;
trialTypes = [0.0; -1.0; 0.0; 1.0];
nTrialTypes = size(trialTypes,1);
repetitions = 50; % one value, or one value per trial type
practiceRepetitions = 10; 
prefix = {'test'} %{'train';'a';'b';'c'}; 
                  % if withinBlock = 0, then one value per trial type, otherwise any
                  % value(s).
practicePrefix = 'p_'; %  

movementRate = 8;
movementNoise = 0;
bufferZone = 50;
predictedMovieFrameDuration = 13.33;
blankDurations = [0; 23; 23; 23];

if withinBlock == 1
   trialTypesMasterList = repmat(trialTypes, 1, repetitions);
   trialTypesPracticeList = repmat(trialTypes, 1, practiceRepetitions);
end;


%MainWindow = screen(0, 'OpenWindow', [], [], 8);
imageX=35;
imageY=35;
imageRect = [0 0 imageX imageY];
screenX = 1024;
screenY = 768;
screenRect = [0 0 screenX screenY];
%hz = round(screen(MainWindow, 'FrameRate'));

clear screen;

%  major variables
%frameDuration = refreshesPerFrame/hz;
edgeZone = imageY;
estMovieFrames = ceil(trialDuration*1000/predictedMovieFrameDuration);
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
% startCoordinates = zeros(nBalls, 2);
xdim = 5; ydim = 6;% a 5 by 6 grid of positions
windowX = 100;windowY = 100;% each cell is 125 by 125 pixels
cxy = newGrids(xdim, ydim, windowX, windowY, screenRect);

for sub = subjects
%	initTime = GetSecs;
	kills = 0;
	
   if withinBlock == 0
      cycles = nTrialTypes;
   else
      fprintf(2, 'ERROR: withinBlock != 0 not yet implemented\n');
      return
   end;
   
   for i = 1:cycles
      for prac = [0 1]
         if (withinBlock == 0)
            filename = [prefix{i} num2str(sub)];
            if prac == 0
               shiftFactor = repmat(trialTypes(i), repetitions, 1);
            else
               shiftFactor = repmat(trialTypes(i), practiceRepetitions, 1);
               filename = [practicePrefix filename];
            end
            if (size(blankDurations,1) > 1)
               blankDuration = blankDurations(i);
            else
               blankDuration = blankDurations(1);
            end;
            shiftAmount = ceil((shiftFactor-1.0) * blankDuration);
            nTrials = size(shiftFactor, 1);
            movieFrames = ones(nTrials, 1) * estMovieFrames + shiftAmount;
         end; % if (withinBlock == 0)
         for trial = 1:nTrials
            % compute trajectories
            deathFlag = 999;
            while deathFlag > 0
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

               trajectory = zeros(nDisks, 2, movieFrames(trial));

               deathFlag = 0;
               for f = 1:movieFrames(trial)
                  % while loop waits until a viable trajectory is generated
                  count = 0;
                  count = count+1;
                  noiseDirection = Randi(24, [nDisks 1]); % random directions in 24 degree increments

                  [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, noiseDirection, noiseMagnitude); % adds a noise vector with random direction and noiseFactor*magnitude magnitude to the signal vector
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

                  [finalTheta, finalMagnitude] = addNoiseVector(direction, magnitude, noiseDirection, noiseMagnitude); % adds a noise vector with random direction and noiseFactor*magnitude magnitude to the signal vector
                  newCoordinates = computeCoordinates(oldCoordinates, finalTheta, finalMagnitude); % recompute coordinates

                  % now double-check to see if there is occlusion or boundary violation

                  for a = 1:nDisks
                     if (newCoordinates(a, 2) > (screenY - imageY))|(newCoordinates(a, 2) < imageY)
                        % floor or ceiling
                        deathFlag = 1;
                        kills = kills +1;
                        % 						['vertical boundary violation with ball ' num2str(a)]
                        % 						['  coordinates (' num2str(newCoordinates(a, 1)) ',' num2str(newCoordinates(a, 2)) ')']
                        % 						['  frame ' num2str(f)']
                        % 						clear screen;
                        % 						return;
                     elseif (newCoordinates(a, 1) > (screenX - imageX))|(newCoordinates(a, 1) < imageX)
                        deathFlag = 2;
                        kills = kills +1;
                        % 						['horizontal boundary violation with ball ' num2str(a)]
                        % 						['  coordinates (' num2str(newCoordinates(a, 1)) ',' num2str(newCoordinates(a, 2)) ')']
                        % 						clear screen;
                        % 						return;
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
                              % 								['occlusion between balls ' num2str(b) ' and ' num2str(c) ' on final frame ']
                              % 								clear screen;
                              % 								return;
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
