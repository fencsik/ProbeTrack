function pathsFile = generator (sInitial)

%%% generator:
%%% generates a set of trajectories for use with Multiple Object Tracking experiments
%%% Authors: David Fencsik (based on file by Todd Horowitz)
%%%
%%% $LastChangedDate$

debug = 0;
stopAfterNKills = 0;

experiment = 'ShiftTrack9';
subjects = 1:10;
%trialDuration = 4;
% Possible trial durations, in seconds
minTrialDuration = 5;
maxTrialDuration = 10;
movementSpeed = 9;
blankDuration = 23;
nDisks = 8;
nTargets = 4;
asynchronous = 0; % 1 = asynchronous, 0 = synchronous
staticReappearance = 1; % 1 = static, 0 = moving; effectively is 0 if asynchronous == 1
responseMode = 1; % 1 = full report, 2 = cue two/pick target, 3 = cue one

%%%%% Define blocks %%%%%
% Session 1
prefix = {'a', 'b', 'c', 'd'};
% Session 2
prefix = {'w', 'x', 'y', 'z'};

numTrialsList = {[8, 64]; [8, 64]; [8, 64]; [8, 64]; };

% % Training blocks 
% blankDuration = 0;
% prefix = {'train'};
% numTrialsList = {[30, 0]};

% testing design
subjects = 1;
prefix = {'a', 'b'};
numTrialsList = {[5, 45]; [5, 45]};
staticReappearance = 0;
asynchronous = 1;

%%%%% Define IVs %%%%%
minListLength = 3; % 3 levels of reappearance position
numBlocks = length(prefix);
for b = 1:numBlocks
end;

fprintf(2, 'Ready to generate %d block(s) for %d subject(s).\n', numBlocks, length(subjects));
fprintf(2, 'Press any key to begin, ''q'' to exit...\n');
c = GetChar;

if c == 'q' | c == 'Q'
   return;
else
   fprintf(2, '\nBeginning path-file generation...\n\n');
end;

starttime = clock;


bufferZone = 200;

% timing variables
predictedFrameDuration = 13.33;
maxFrames = ceil(maxTrialDuration*1000/predictedFrameDuration);
minFrames = ceil(minTrialDuration*1000/predictedFrameDuration);

% size/distance variables
%MainWindow = screen(0, 'OpenWindow', [], [], 8);
screenX = 1024;
screenY = 768;
diskDiameter = 40;
rectScreen = [0 0 screenX screenY];
rectDisplay = CenterRect([0, 0, screenX - diskDiameter, screenY - diskDiameter], rectScreen);

for sub = subjects
   %	initTime = GetSecs;
   restarts = 0;
   
   for block = 1:numBlocks
      filename = [prefix{block} num2str(sub)];
      numTrials = numTrialsList{block};
      pracTrials = 0;
      if length(numTrials) > 1
         pracTrials = numTrials(1);
         numTrials = numTrials(2);
      end
      numReps = ceil(numTrials / minListLength);
      listLength = minListLength * numReps;
      if listLength > numTrials
         fprintf(2, 'WARNING: Unbalanced trials in block %d\n\n', b);
      end
      
      % construct a list that balances reappearance position
      shift = [-1; 0; 1];
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
      velocity = (movementSpeed*30)*(predictedFrameDuration)/1000;

      % use kills to keep track of where problems are occuring
      kills = zeros(1,2);

      startPositions = zeros(nDisks, 2, numTrials);
      startDirections = zeros(nDisks, numTrials);
      blankStarts = zeros(nDisks, numTrials);
      
      for trial = 1:numTrials
         blankWindowStart = round(2000/predictedFrameDuration);
         blankWindowEnd   = numFrames(trial) - round(1000/predictedFrameDuration);
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
            else
               nStarts = [5 5]; % ncol, nrow
               delta = [RectWidth(rectDisplay) / (nStarts(1) + 1), RectHeight(rectDisplay) / (nStarts(2) + 1)];
               pos = [repmat((1:nStarts(1))', nStarts(2), 1), ...
                      reshape(repmat(1:nStarts(2), nStarts(1), 1), prod(nStarts), 1)];
               [tmp, index] = sort(rand(size(pos, 1), 1)); 
               pos = pos(index(1:nDisks), :) .* repmat(delta, nDisks, 1);
               startPositions(:, :, trial) = pos;
            end

            % finished finding a good set of starting points

            theta = rand(nDisks, 1) * 2 * pi;
            startDirections(:, trial) = theta;
            delta = [velocity .* cos(theta), -1 .* velocity .* sin(theta)];

            reappearancePositions = zeros(nDisks, 2);

            f = 1;
            while f <= numFrames(trial)
               noBouncing = zeros(nDisks, 1);
               
               % mark disks that shouldn't bounce
               index = f > blankStarts(:, trial) - blankDuration - 1 & f <= blankEnd;
               if blankDuration > 0 & any(index)
                  noBouncing(index) = 1;
               end;

               nextX = pos(:, 1) + delta(:, 1);
               nextY = pos(:, 2) + delta(:, 2);
               bounceX = nextX < rectDisplay(RectLeft) | nextX > rectDisplay(RectRight);
               bounceY = nextY < rectDisplay(RectTop) | nextY > rectDisplay(RectBottom);
               % make sure the noBouncing disks don't bounce
               if any(bounceX | bounceY) & any(noBouncing(bounceX | bounceY))
                  % at least one bouncing disk cannot be bouncing on this frame
                  deathFlag = 1;
                  kills(deathFlag) = kills(deathFlag) + 1;
                  if debug, fprintf('failed at step %d, frame %d\n', deathFlag, f); end
                  break;
               end
               if any(bounceX)
                  delta(bounceX, 1) = -1 * delta(bounceX, 1);
               end
               if any(bounceY)
                  delta(bounceY, 2) = -1 * delta(bounceY, 2);
               end

               pos = pos + delta;

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

               % if f is the final frame, then check that everything is far enough apart
               if f == numFrames(trial)
                  D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                           (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
                  if any(D > 0 & D < bufferZone)
                     % end point is no good
                     deathFlag = 2;
                     kills(deathFlag) = kills(deathFlag) + 1;
                     if debug, fprintf(1, 'failed at step %d, frame %d\n', deathFlag, f); end
                     break;
                  end
               end
               f = f + 1;
            end % while f <= numFrames(trial)
         end % while deathFlag > 0
      end % for trial = 1:numTrials

      save (filename, 'experiment', 'numFrames', 'startPositions', 'startDirections', ...
            'shift', 'blankStarts', 'velocity', 'rectScreen', 'rectDisplay', 'diskDiameter', ...
            'movementSpeed', 'blankDuration', 'pracTrials', 'nTargets', ...
            'asynchronous', 'responseMode', 'predictedFrameDuration');

      fprintf('***** Finished %s *****\nKills by category:\n', filename);
      disp([1:(size(kills,2)); kills]);
   end; % for block = 1:numBlocks
end; % for sub = subjects

fprintf('\nElapsed time = %1.2f sec\n\n', etime(clock, starttime));


