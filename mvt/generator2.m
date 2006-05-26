function generator2

% Author: David E. Fencsik
% 
% $LastChangedDate$

fileName = 'StopTrack6Paths.mat';
backupFile = 'backup.mat';

% basic parameters
nPaths = 2;
nDisks = 10;
durationFlag = 1; % 1 = range of durations; 2 = discrete durations, with nPaths in each
startsFlag = 2; % 1 = start from grid; 2 = start from random positions
blankFlag = 1; % 0 = no blank; 1 = blank at end
durationList = [150 450];
blankDuration = 23;

% specify velocity in pixels/frame
% for speed V in deg/sec, multiply by (30 pix/deg * 1/75 sec/frames)
%%%velocity = 3.6; % 9 deg/sec
velocity = 3.2; % 8 deg/sec

% display settings
rectScreen = [0 0 1024 768];
rectDisplay = [0 0 600 600];
bufferZone = 60; % min center-to-center distance at critical points in trial; pixels
diskRadius = 20; % pixels

% initialize RNG
rngseed = sum(100*clock);
rand('state',rngseed);

% set up durations
if durationFlag == 1
   durationList = min(durationList):max(durationList);
   nDurations = length(durationList);
   pathDurations = durationList(Randi(nDurations, [nPaths, 1]))';
elseif durationFlag == 2
   nDurations = length(durationList);
   pathDurations = reshape(repmat(reshape(durationList, [1, nDurations]), [nPaths, 1]), [nPaths * nDurations, 1]);
   nPaths = nPaths * nDurations;
else
   error(sprintf('duration flag %d not supported', durationFlag));
end
startPositions = zeros(nDisks, 2, nPaths);
startVelocities = zeros(nDisks, 2, nPaths);

% more set-up of display
sizeX = RectWidth(rectDisplay) - 2 * diskRadius;
sizeY = RectHeight(rectDisplay) - 2 * diskRadius;
rectBoundary = CenterRect([0, 0, sizeX, sizeY], rectDisplay);

% set up error codes
errBlankBounce = 1;
errPreBlankCloseness = 2;
errEndTrialCloseness = 3;
nErrors = 3;
errorCodes = {'Bounced during blank', ...
              'Too close before blank', ...
              'Too close at trial end'};
errorCount = zeros(1, nErrors);

for p = 1:nPaths
   regenerateLocations = 0;
   badPath = 1;
   counter = 0;
   while badPath
      badPath = 0;
      counter = counter + 1;
      if mod(counter, 100) == 0
         disp(errorCount);
      end
      if regenerateLocations > 0
         regenerateLocations = regenerateLocations - 1;
         pos = lastGoodStartingPositions;
      else
         D = 1;
         n = 0;
         while any(any(D > 0 & D < bufferZone))
            pos = [Randi(sizeX, [nDisks, 1]) + diskRadius, Randi(sizeY, [nDisks, 1]) + diskRadius];
            D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                     (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
            n = n + 1;
            if n > 1000
               error('took too long to find a good set of starting positions');
            end
         end
         lastGoodStartingPositions = pos;
         regenerateLocations = 100;
      end

      theta = rand(nDisks, 1) * 2 * pi;
      delta = [velocity .* cos(theta), -1 .* velocity .* sin(theta)];
      lastGoodStartingVelocities = delta;

      for f = 2:pathDurations(p)
         % test future positions
         next = pos + delta;
         bounceX = (next(:, 1) < rectBoundary(RectLeft) | next(:, 1) > rectBoundary(RectRight))';
         bounceY = (next(:, 2) < rectBoundary(RectTop) | next(:, 2) > rectBoundary(RectBottom))';
         
         % make sure there's no bouncing during the gap
         if blankDuration > 0 & f >= pathDurations(p) - blankDuration & any(bounceX | bounceY)
           badPath = 1;
           errorCount(errBlankBounce) = errorCount(errBlankBounce) + 1;
           break;
         end

         % bounce disks (reverse appropriate directions of motion)
         if any(bounceX), delta(bounceX, 1) = -1 * delta(bounceX, 1); end
         if any(bounceY), delta(bounceY, 2) = -1 * delta(bounceY, 2); end
         
         pos = pos + delta;
         
         % if this is the final pre-gap frame, check min distances
         if blankDuration > 0 & f == pathDurations(p) - blankDuration - 1
            D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                     (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
            if any(any(D > 0 & D < bufferZone))
               badPath = 1;
               errorCount(errPreBlankCloseness) = errorCount(errPreBlankCloseness) + 1;
               break;
            end
         end

         % if this is the final frame, check min distances
         if f == pathDurations(p)
            D = sqrt((repmat(pos(:, 1), [1, nDisks]) - repmat(pos(:, 1)', [nDisks, 1])) .^ 2 + ...
                     (repmat(pos(:, 2), [1, nDisks]) - repmat(pos(:, 2)', [nDisks, 1])) .^ 2);
            if any(any(D > 0 & D < bufferZone))
               badPath = 1;
               errorCount(errEndTrialCloseness) = errorCount(errEndTrialCloseness) + 1;
               break;
            end
         end

      end % for f = 2:pathDurations(p)
   end % while badPath
   startPositions(:, :, p) = lastGoodStartingPositions;
   startVelocities(:, :, p) = lastGoodStartingVelocities;
end % for p = 1:nPaths

% report errors
for e = 1:nErrors
   fprintf('%s : %d\n', errorCodes{e}, errorCount(e));
end

save(fileName, 'startPositions', 'startVelocities', 'pathDurations', 'blankDuration', ...
     'velocity', 'rectDisplay', 'bufferZone', 'diskRadius', ...
     'durationFlag', 'startsFlag', 'blankFlag');


