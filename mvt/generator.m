function pathsFile = generator (sInitial)

%%% generator:
%%% generates a set of trajectories for use with Multiple Object Tracking experiments
%%% Authors: David Fencsik (based on file by Todd Horowitz)
%%%
%%% $LastChangedDate$

experiment = 'StopTrack5';

debug = 0;
stopAfterNKills = 0;

subjects = 1:8;
% Possible trial durations, in seconds
minTrialDuration = 3; % sec
maxTrialDuration = 6; 
movementSpeed = 8; % deg/sec
blankDuration = .3; % sec
nDisks = 10;

asynchronous = 0; % 1 = asynchronous, 0 = synchronous
staticReappearance = 1; % 1 = static, 0 = moving; effectively is 0 if asynchronous == 1
responseMode = 1; % 1 = full report, 2 = cue two/pick target, 3 = cue one
randomStarts = 1; % 1 = random starting positions, 0 = selected from grid

%%%%% Define blocks %%%%%
prefix = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'};
numTrialsList = {[8, 32]; [8, 32]; [8, 32]; [8, 32]; ...
                 [8, 32]; [8, 32]; [8, 32]; [8, 32];};

% % Training blocks 
% blankDuration = 0;
% prefix = {'train'};
% numTrialsList = {[30, 0]};

% testing design
% subjects = 2;
% prefix = {'test'};
% numTrialsList = {1};

numBlocks = length(prefix);

fprintf(2, 'Ready to generate %d block(s) for %d subject(s).\n', numBlocks, length(subjects));
fprintf(2, 'Press ''b'' to begin, ''q'' to quit...\n');
FlushEvents('keyDown');
c = lower(GetChar);

while ~(c == 'q' | c == 'b')
   FlushEvents('keyDown');
   c = lower(GetChar);
end
if c == 'q' | c == 'Q'
   return;
end

fprintf(2, '\nBeginning path-file generation...\n\n');
starttime = clock;

% timing variables
predictedFrameDuration = .01333; % sec / frame
blankDuration = ceil(blankDuration / predictedFrameDuration);
maxFrames = ceil(maxTrialDuration / predictedFrameDuration);
minFrames = ceil(minTrialDuration / predictedFrameDuration);

% size/distance variables
%MainWindow = screen(0, 'OpenWindow', [], [], 8);
screenX = 800;
screenY = 600;
diskDiameter = 40;
rectScreen = [0 0 screenX screenY];
rectBoundary = CenterRect([0, 0, screenX - diskDiameter, screenY - diskDiameter], rectScreen);
bufferZone = 80;

for sub = subjects
   restarts = 0;
   
   for block = 1:numBlocks
      filename = [prefix{block} num2str(sub)];
      numTrials = numTrialsList{block};
      pracTrials = 0;
      if length(numTrials) > 1
         pracTrials = numTrials(1);
         numTrials = numTrials(2);
      end
      minListLength = 1; % 1 level of reappearance position
      numReps = ceil(numTrials / minListLength);
      listLength = minListLength * numReps;
      if listLength > numTrials
         fprintf(2, 'WARNING: Unbalanced trials in block %d\n\n', block);
      end
      
      % construct a list that balances reappearance position
      shift = [1];
      % extend the lists as needed
      if numReps > 1
         shift = repmat(shift, [numReps, 1]);
      end
      shift = Shuffle(shift);
      % generate practice trial types (random)
      if pracTrials > 0
         pracShift = shift(Randi(length(shift), [pracTrials, 1]));
         shift = [pracShift; shift];
      end

      numTrials = numTrials + pracTrials;
      
      % determine trial length (in frames)
      numFrames = minFrames + Randi(maxFrames - (minFrames-1), [numTrials,1]) - 1;

      % movement vector length
      velocity = movementSpeed * 30 * predictedFrameDuration;

      % use kills to keep track of where problems are occuring
      kills = zeros(1,4);

      startPositions = zeros(nDisks, 2, numTrials);
      startDirections = zeros(nDisks, numTrials);
      blankStarts = zeros(nDisks, numTrials);
      
      for trial = 1:numTrials
         blankWindowStart = round(2 / predictedFrameDuration);
         blankWindowEnd   = numFrames(trial) - round(1 / predictedFrameDuration);
         if asynchronous == 1
            % Pick random blank intervals for each disk
            frames = Shuffle(blankWindowStart:blankWindowEnd);
            blankStarts(:, trial) = frames(1:nDisks)';
         else
            if staticReappearance == 1
               % The blank interval occurs blankDuration frames before the
               % last frame
               blankStarts(:, trial) = repmat(numFrames(trial) - blankDuration, [nDisks, 1]);
            else
               % Pick one random blank interval for all disks
               blankStarts(:, trial) = repmat(Randi(blankWindowEnd - blankWindowStart + 1, 1) + blankWindowStart - 1, ...
                                              [nDisks, 1]);
            end;
         end;
         blankEnd = blankStarts(:, trial) + blankDuration;

         restarts = 0;
         
         redoLocations = 100;
         redoLocationsCounter = 0;

         deathFlag = 999;
         while deathFlag > 0
            % quit if we've been going for too long:
            if stopAfterNKills > 0 & sum(kills) > stopAfterNKills * numTrials
               kills = [1:(size(kills, 2)); kills]
               fprintf(1, 'Too many kills.\n');
               fprintf(1, 'Last kill = %d\n', deathFlag);
               return;
            end;

            % while loop waits until a viable trajectory is generated
            deathFlag = 0;

            % starting positions
            if redoLocationsCounter > 0
               % keep the same locations and pick new directions
               redoLocationsCounter = redoLocationsCounter - 1;
               pos = lastGoodStartingPositions;
            else
               redoLocationsCounter = redoLocations;
               if randomStarts == 0
                  nStarts = [5 5]; % ncol, nrow
                  delta = [RectWidth(rectBoundary) / (nStarts(1) + 1), RectHeight(rectBoundary) / (nStarts(2) + 1)];
                  pos = [repmat((1:nStarts(1))', nStarts(2), 1), ...
                         reshape(repmat(1:nStarts(2), nStarts(1), 1), prod(nStarts), 1)];
                  [tmp, index] = sort(rand(size(pos, 1), 1)); 
                  pos = pos(index(1:nDisks), :) .* repmat(delta, nDisks, 1);
               else
                  D = 1;
                  counter = 0;
                  while any(any(D > 0 & D < bufferZone))
                     pos = [Randi(RectWidth(rectBoundary), [nDisks, 1]) + rectBoundary(RectLeft), ...
                            Randi(RectHeight(rectBoundary), [nDisks, 1]) + rectBoundary(RectTop)];
                     D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                              (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
                     counter = counter + 1;
                     if counter > 1000
                        error('took too long to find a good set of random positions');
                     end
                  end
                  kills(4) = kills(4) + counter;
               end
               lastGoodStartingPositions = pos;
            end
            startPositions(:, :, trial) = pos;
            % finished finding a good set of starting points

            theta = rand(nDisks, 1) * 2 * pi;
            startDirections(:, trial) = theta;
            delta = [velocity .* cos(theta), -1 .* velocity .* sin(theta)];

            reappearancePositions = zeros(nDisks, 2);

            for f = 1:numFrames(trial)
               if blankDuration > 0
                  index = 0;
                  % mark positions of any disks that are in their post-gap reappearance positions
                  switch shift(trial)
                   case -1
                     index = f == blankStarts(:, trial) - blankDuration - 1;
                   case 0
                     index = f == blankStarts(:, trial) - 1;
                   case 1
                     index = f == blankEnd;
                   otherwise
                     error(['unknown shift ', num2str(shift), ' requested.']);
                  end
                  if any(index)
                     reappearancePositions(index, :) = pos(index, :);
                  end

                  % At the end of each disk's blank interval, adjust its reappearance
                  % position according to the current trialType
                  index = f == blankEnd;
                  if any(index)
                     pos(index, :) = reappearancePositions(index, :);
                  end
               end
               
               % if disappearance is synchronous and it's the last pre-gap frame,
               % check min distances
               if blankDuration > 0 & asynchronous == 0 & f == blankStarts(1, trial)
                  D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                           (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
                  if any(any(D > 0 & D < bufferZone))
                     % pre-gap locations are no good
                     deathFlag = 3;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     if debug, fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f); end
                     break;
                  end
               end
                  

               % if f is the final frame, then check that everything is far enough apart
               if f == numFrames(trial)
                  D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                           (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
                  if any(any(D > 0 & D < bufferZone))
                     % end point is no good
                     deathFlag = 2;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     if debug, fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f); end
                     break;
                  end
               end

               nextX = pos(:, 1) + delta(:, 1);
               nextY = pos(:, 2) + delta(:, 2);
               bounceX = nextX < rectBoundary(RectLeft) | nextX > rectBoundary(RectRight);
               bounceY = nextY < rectBoundary(RectTop) | nextY > rectBoundary(RectBottom);

               % mark disks that shouldn't bounce
               if blankDuration > 0
                  noBouncing = zeros(nDisks, 1);
                  index = f >= blankStarts(:, trial) & f <= blankEnd;
                  if any(index)
                     noBouncing(index) = 1;
                  end
                  % make sure the noBouncing disks don't bounce
                  if any(bounceX | bounceY) & any(noBouncing(bounceX | bounceY))
                     % at least one bouncing disk cannot be bouncing on this frame
                     deathFlag = 1;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     if debug, fprintf('failed at step %d, frame %d\n', deathFlag, f); end
                     break;
                  end
               end
               if any(bounceX)
                  delta(bounceX, 1) = -1 * delta(bounceX, 1);
               end
               if any(bounceY)
                  delta(bounceY, 2) = -1 * delta(bounceY, 2);
               end

               pos = pos + delta;
            end % for f = 1:numFrames(trial)
         end % while deathFlag > 0
      end % for trial = 1:numTrials

      save (filename, 'experiment', 'numFrames', 'startPositions', 'startDirections', ...
            'shift', 'blankStarts', 'velocity', 'rectScreen', 'rectBoundary', 'diskDiameter', ...
            'movementSpeed', 'blankDuration', 'pracTrials', ...
            'asynchronous', 'staticReappearance', 'responseMode', 'predictedFrameDuration');

      fprintf('***** Finished %s *****\nKills by category:\n', filename);
      disp([1:(size(kills,2)); kills]);
   end; % for block = 1:numBlocks
end; % for sub = subjects

fprintf('\nElapsed time = %1.2f sec\n\n', etime(clock, starttime));


