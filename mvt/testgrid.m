function testgrid()

imageX=35;
imageY=35;
imageRect = [0 0 imageX imageY];
coreRect = imageRect - [0 0 4 4];
ballRect = CenterRect(coreRect, imageRect);
MainWindow = screen(0, 'OpenWindow', [], [], 8);
screenRect = screen(0, 'Rect');
screenX = screenRect(3);
screenY = screenRect(4);
gridRect = [0 0 screenX-70 screenY-70];
gridRect = CenterRect(gridRect, imageRect);

white = WhiteIndex(MainWindow);
black = BlackIndex(MainWindow);
gray = round((white+black)/2);
if round(gray)==white
	gray=black;
end
darkGray = (gray+black)/2;
lightGray = (gray+white)/2;
% now set colors
red = 1;
blue = 2;
green = 3;
yellow = 4;
clut = screen(MainWindow, 'GetClut');
clut (red + 1, :) = [255 0 0];
clut (blue + 1, :) = [0 0 255];
clut (green + 1, :) = [0 255 0];
clut (yellow +1, :) = [255 255 0];
LoadClut(MainWindow, clut);

screen(MainWindow, 'FillRect', gray);

centerrect(imageRect, screenRect);% centers first rect in second rect
KbCheck; % checks whether a key is pressed down
GetSecs; % returns the number of seconds since the computer started

ballPointer = screen(MainWindow, 'OpenOffScreenWindow', [], imageRect);
screen(ballPointer, 'FillOval', black, imageRect);
screen(ballPointer, 'FillOval', darkGray, ballRect);

eraserPointer = screen(MainWindow, 'OpenOffScreenWindow', [], imageRect);
screen(eraserPointer, 'FillOval', gray, imageRect);


% set the state of the random number generator to some random value (based on the clock)
tau = clock;
rand('state',sum(100*tau));
state=rand('state');



mygrid = rotategrid(5, 3, gridRect);
n = size(mygrid,1);

for i = 1:n
   screen('CopyWindow', ballPointer, , imageRect, placeRect{n}, 'transparent');
