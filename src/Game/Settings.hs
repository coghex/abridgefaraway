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
-- 30 and 60 should work
fps = 30.0::Double
-- the bigger the map the more zoom you need, soon to be controlled by keys
--zoom = 120::Float
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
history = 12::Int

-- how hard it is to heat the oceans
specificheatofwater = 1009::Float
-- how hard it is to heat the earth
specificheatofterra = 777::Float
-- how hard it is to heat air
specificheatofair   = 200::Float
-- how hard it is to move the oceans
momentumofwater     = 912::Float
-- how hard it is to move the air
momentumofair       = 200::Float
-- the amount of light that goes throught the water
clarityofwater      = 0.5::Float
-- the strength of the tides
tidalstrength       = 0.2::Float
-- the strength of the coriolis effect on fluids
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
-- the temp at which humans prefer
idealtemp           = 20.0::Float

-- these settings effect the zones:
-- width and height
zonew = 60::Int
zoneh = 45::Int
-- the number of tiles for a zone
ntiles = 384::Int
-- the maximum number of zazz objects in a zone
nzazz = 4::Int
-- this is the maximum number of zazz objects of each size in a zone, and their respective sizes
zazzcounts = [0, 0, 1, 0, 0,  1, 0, 1, 0]::[Int]
zazzsizes  = [(16, 16), (16, 8), (8, 8), (8, 4), (4, 4), (4, 3), (3, 3), (3, 2), (1, 1)]::[(Int, Int)]
-- this is the number of volcano tiles i have made at each size so far
volccounts = [0, 0, 10, 0, 0,  2, 0, 1, 0]::[Int]
-- this is the null tile, special tile, and the blank tile
nulltile = 111::Int
specialtile = 224::Int
blanktile = 223::Int

-- these settings will effect units
-- the steps at which the pathing algorithm will work
pathstep = 0.1::Float
