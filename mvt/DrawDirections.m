function DrawDirections(numdir)
% DrawDirections(numdir)
% 
% Draws a series of lines representing numdir equally
% spaced directions around a circle. The first direction
% is north.
   
   circle = 2 * pi;
   quarter = circle / 4;
   radius = .5;
   delta = circle / numdir;
   
   % draw a reference cross
   rref = 1.2;
   raxis = 1.25;
   rtext = 1.05;
   plot([-rref*radius, rref*radius], [0, 0], 'c-');
   axis([-raxis*radius, raxis*radius, -raxis*radius, raxis*radius]);
   hold on;
   plot([0, 0], [-rref*radius, rref*radius], 'c-');

   for n = 1:numdir
      theta = n * delta;
      x = (radius) * sin(theta);
      y = (radius) * cos(theta);
      plot([0, x], [0, y], 'k-');
      text([0, x*rtext], [0, y*rtext], num2str(n));
   end;
   
%   plot([0, 0], [.4330, -.25], 'ko);
   hold off;
   
%%% END DrawDirections %%%