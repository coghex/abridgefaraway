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
-- the number of tiles for a zone
ntiles = 111::Int
-- the list of tiles and how they fit together
nfitlist = [
           [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --0
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --1
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --2
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --3
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --4
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --5
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --6
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --7
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --8
         , [6, 9, 72, 87, 91]                                      --9
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --10
         , [8, 11, 74, 88, 89]                                     --11
         , [6, 9, 72, 87, 91]                                      --12
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --13
         , [8, 11, 74, 88, 89]                                     --14
         , [15, 16, 17, 22, 82, 85, 94]                            --15
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --16
         , [15, 16, 17, 22, 82, 85, 94]                            --17
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --18
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --19
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --20
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --21
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --22
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --23
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --24
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --25
         , [15, 16, 17, 22, 82, 85, 94]                            --26
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --27
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --28
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --29
         , [27, 30]                                                --30
         , [28, 31, 69, 70, 71]                                    --31
         , [29, 32]                                                --32
         , [27, 30]                                                --33
         , [28, 31, 69, 70, 71]                                    --34
         , [29, 32]                                                --35
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --36
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --37
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --38
         , [36, 39, 98, 102, 104, 105]                             --39
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --40
         , [38, 41, 95, 100, 103, 107]                             --41
         , [36, 39, 98, 102, 104, 105]                             --42
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --43
         , [38, 41, 95, 100, 103, 107]                             --44
         , [45, 46, 47, 101, 108, 109, 110]                        --45
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --46
         , [45, 46, 47, 101, 108, 109, 110]                        --47
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --48
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --49
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --50
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --51
         , [45, 46, 47, 101, 108, 109, 110]                        --52
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --53
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --54
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --55
         , [56]                                                    --56
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --57
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --58
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --59
         , [57]                                                    --60
         , [58]                                                    --61
         , [59]                                                    --62
         , [60]                                                    --63
         , [61]                                                    --64
         , [62]                                                    --65
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --66
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --67
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --68
         , [28, 31, 69, 70, 71]                                    --69
         , [28, 31, 69, 70, 71]                                    --70
         , [28, 31, 69, 70, 71]                                    --71
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --72
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --73
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --74
         , [7, 73]                                                 --75
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --76
         , [7, 73]                                                 --77
         , [6, 9, 72, 87, 91]                                      --78
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --79
         , [8, 11, 74, 88, 89]                                     --80
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --81
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --82
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --83
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --84
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --85
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --86
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --87
         , [6, 9, 72, 87, 91]                                      --88
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --89
         , [6, 9, 72, 87, 91]                                      --90
         , [8, 11, 74, 88, 89]                                     --91
         , [8, 11, 74, 88, 89]                                     --92
         , [38, 41]                                                --93
         , [7, 10, 66, 67, 68, 73, 76, 90, 92, 97]                 --94
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --95
         , [36, 39]                                                --96
         , [15, 16, 17, 22, 82, 85, 94]                            --97
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --98
         , [45, 46, 47]                                            --99
         , [36, 39]                                                --100
         , [0, 1, 2, 3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 33, 34, 35, 37, 40, 42, 43, 44, 48, 49, 50, 51, 52, 53, 54, 55, 78, 79, 80, 81, 82, 83, 84, 86, 93, 96, 99] --101
         , [36, 39]                                                --102
         , [38, 41]                                                --103
         , [38, 41]                                                --104
         , [45, 46, 47]                                            --105
         , [56]                                                    --106
         , [45, 46, 47]                                            --107
         , [36, 39]                                                --108
         , [45, 46, 47]                                            --109
         , [38, 41]                                                --110
           ]::[[Int]]

sfitlist = [
           [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --0
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --1
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --2
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --3
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --4
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --5
         , [9, 12, 78, 88, 90]                                    --6
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --7
         , [11, 14, 80, 91, 92]                                   --8
         , [9, 12, 78, 88, 90]                                    --9
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --10
         , [11, 14, 80, 91, 92]                                   --11
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --12
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --13
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --14
         , [15, 17, 26, 97]                                       --15
         , [15, 17, 26, 97]                                       --16
         , [15, 17, 26, 97]                                       --17
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --18
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --19
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --20
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --21
         , [15, 17, 26, 97]                                       --22
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --23
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --24
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --25
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --26
         , [30, 33]                                               --27
         , [31, 34, 69, 70, 71]                                   --28
         , [32, 35]                                               --29
         , [30, 33]                                               --30
         , [31, 34, 69, 70, 71]                                   --31
         , [32, 35]                                               --32
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --33
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --34
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --35
         , [39, 42, 96, 100, 102, 108]                            --36
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --37
         , [41, 44, 93, 103, 104, 110]                            --38
         , [39, 42, 96, 100, 102, 108]                            --39
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --40
         , [39, 42, 96, 100, 102, 108]                            --41
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --42
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --43
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --44
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --45
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --46
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --47
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --48
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --49
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --50
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --51
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --52
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --53
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --54
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --55
         , [56]                                                   --56
         , [60]                                                   --57
         , [61]                                                   --58
         , [62]                                                   --59
         , [63]                                                   --60
         , [64]                                                   --61
         , [65]                                                   --62
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --63
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --64
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --65
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --66
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --67
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --68
         , [31, 34, 69, 70, 71]                                   --69
         , [31, 34, 69, 70, 71]                                   --70
         , [31, 34, 69, 70, 71]                                   --71
         , [9, 12, 78, 88, 90]                                    --72
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --73
         , [11, 14, 80, 91, 92]                                   --74
         , [12, 75, 78, 88, 90]                                   --75
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --76
         , [14, 77, 80, 91, 92]                                   --77
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --78
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --79
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --80
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --81
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --82
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --83
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --84
         , [15, 17, 26, 97]                                       --85
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --86
         , [9, 12, 78, 88, 90]                                    --87
         , [11, 14, 80, 91, 92]                                   --88
         , [11, 14, 80, 91, 92]                                   --89
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --90
         , [9, 12, 78, 88, 90]                                    --91
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --92
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --93
         , [15, 17, 26, 97]                                       --94
         , [36, 37, 38, 40, 41, 44, 46, 48, 49, 50, 51, 53]
         , [0, 1, 2, 3, 4, 5, 6, 7, 8, 16, 18, 19, 20, 21, 22, 23, 27, 28, 29, 36, 37, 38, 40, 43, 46, 48, 49, 50, 51, 53, 72, 73, 74, 76, 81, 82, 83, 84, 85, 86, 95, 98, 101]      --96
         , [10, 13, 24, 25, 54, 55, 66, 67, 68, 79, 87, 89, 94]   --97
           ]::[[Int]]
    
efitlist = [
           []
           ]::[[Int]]

wfitlist = [
           []
           ]::[[Int]]
