-- base mod

-- imports all the settings as well as defines some
-- basic gameplay

-- this is the screen width and height in pixels
screenw = 2560
screenh = 1440

-- weather or not the game is fullscreen
fullscreen = false

-- frames per second of display
fps = 30.0

-- will return a hard coded default screen size of 1024x768
-- if one is not set
function getscreensize (dscreenw, dscreenh)
    if fullscreen or screenw == nil or screenh == nil then 
        return {dscreenw, dscreenh}
    else
        return {screenw, screenh}
    end
end
