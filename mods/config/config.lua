-- config mod

-- imports all the settings

-- this is the screen width and height in pixels
screenw = 800
screenh = 600

-- weather or not the game is fullscreen
fullscreen = false

-- the size of the fonts
fontsize = 40

-- frames per second of display
fps = 30.0

-- the speed of time, and animation
timespeed = 1000
animspeed = 100

-- the amount of history that passes before the world is presented
history = 12

-- the precision of floating point numbers
precision = 2

-- these constants help the world generation
-- size of the world
gridw = 40
gridh = 30
-- fudges lists when they need to be slightly altered
fudge = 4
-- all arcane ingredients in the worldgen pie
salt = 4
sugar = 4.0
vigor = 1
-- amount of continents generated, some continents
-- are big landmass, others are islands, valleys,
-- seas, trenches and ridges
minnconts = 10
maxnconts = 80
-- size of created continents
minsize   = 20
maxsize   = 800
-- amount of spots created for each continent
minnspots = 1
maxnspots = 10
-- sealevel above base sea floot
sealevel = 2000.0
-- level before tiles become impassable mountains
peaklevel = 10000.0

-- will return a hard coded default screen size of 1024x768
-- if one is not set
function getscreensize (dscreenw, dscreenh)
    if fullscreen or screenw == nil or screenh == nil then 
        return {dscreenw, dscreenh}
    else
        return {screenw, screenh}
    end
end
