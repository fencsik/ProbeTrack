function coordinates = rotategrid(ncol, nrow, rect)
% rotategrid(ncol, nrow, rect)
%    ncol = # of columns in the grid
%    nrow = # of rows in the grid
%    rect = bounding rectangle for grid
%
% Generates a ncol x nrow grid that will fit in rect 
% under any rotation, then rotates it randomly and
% returns the rotated coordinates.

   if rect(1) ~= 0
      diff = rect(1);
      rect(1) = 0;
      rect(3) = rect(3) - diff;
   end;
   if rect(2) ~= 0
      diff = rect(2);
      rect(2) = 0;
      rect(4) = rect(4) - diff;
   end;

   size = min(rect(rect > 0)) / sqrt(2);
   xOffset = size / (ncol - 1);
   yOffset = size / (nrow - 1);
   gridRect = [0 0 size size];
   [newRect dx dy] = CenterRect(gridRect, rect);

   elements = nrow*ncol;
   coordinates = zeros (elements, 2);
   i = 1:elements; % index variable

   y = fix((i-1)./nrow)+1;
   x = mod(i-1,ncol)+1;

   y = y';
   x = x';

   coordinates (:, 1) = (x*xOffset)+dx;
   coordinates (:, 2) = (y*yOffset)+dy;
