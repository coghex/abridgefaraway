module Game.Settings where

-- this holds constants for things that i change on occasion.
-- in future each of these should have in game change functions

-- grid sizes should work for maps larger that the fudge factor
-- grids larger than 120x120 will begin to make worldgen slow
gridw = 60::Int
gridh = 40::Int
-- should work for most standard screen sizes 720 or greater
screenw = 2560::Int
screenh = 1440::Int
-- the bigger the map the more zoom you need, soon to be controlled by keys
zoom = 150::Float
-- fudges the world to keep the edges together
fudge = 10::Int
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

-- seconds of history that pass before the world is presented
-- there are 36000 seconds in a day, 360 days in a year
history = 2000::Int


-- pretty self explanitory
specificheatofwater = 100000::Float
terratemp = 14.6::Float
