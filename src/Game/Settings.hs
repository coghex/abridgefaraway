module Game.Settings where

-- this holds constants for things that i change on occasion.
-- in future each of these should have in game change functions

-- grid sizes should work for maps larger that the fudge factor
-- grids larger than 120x120 will begin to make worldgen slow
-- below around 12x8 the game will crash
gridw = 40::Int
gridh = 30::Int
-- should work for most standard screen sizes 720 or greater
screenw = 2560::Int
screenh = 1440::Int
-- the bigger the map the more zoom you need, soon to be controlled by keys
zoom = 120::Float
-- the precision of floating point numbers
precision = 2::Int
-- fudges various lists to avoid NPE
fudge = 4::Int
-- all arcane ingredients in the worldgen pie
salt = 4::Int
sugar = 4::Float
vigor = 1::Int

-- sealevel above base sea floor
sealevel = 2000::Float
-- level before all tiles become mountains
peaklevel = 10000::Float
-- times to run the quasi-blur function on the elevation
erosion = 3::Int
-- minimum and maximum sizes of the created continents
minsize = 20::Int
maxsize = 800::Int
-- amount of continents generated, some continents are big landmass,
-- others are islands, valleys, ridges, seas, and trenchs.
minnconts = 10::Int
maxnconts = 80::Int
-- amount of spots created for a single continent
minnspots = 1::Int
maxnspots = 10::Int

-- radius of the light from the sun
radius = 20::Int

-- hours of history that pass before the world is presented
-- there are 60 min in an hour, 24 hours in a day, 360 days in a year
history = 200::Int

-- how hard it is to heat the oceans
specificheatofwater = 1009::Float
-- how hard it is to heat the earth
specificheatofterra = 777::Float
-- how hard it is to move the oceans
momentumofwater     = 912::Float
-- the amount of light that goes throught the water
clarityofwater      = 0.5::Float
-- the strength of the tides
tidalstrength       = 0.2::Float
-- the strength of the coriolis effect on water
coriolispower       = 0.1::Float
-- freezing point of water
freezingpoint       = -1.1::Float
-- temperature of the landmass
terratemp           = 14.6::Float
-- temperature at which average salinity water will be at
-- its maximum density
maxdensitytemp      = -1.9::Float
-- the velocity at which currents will display
currentslevel       = 0.02::Float


-- these settings effect the zones:
-- width and height
zonew = 60::Int
zoneh = 45::Int


