-- base mod

-- imports all the settings

-- this is the screen width and height in pixels
screenw = 2560
screenh = 1440

-- weather or not the game is fullscreen
fullscreen = false

function getscreensize (dscreenw, dscreenh)
    if screenw == nil and screenh == nil then 
        return {dscreenw, dscreenh}
    else
        return {screenw, screenh}
    end
end
