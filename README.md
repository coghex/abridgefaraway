# A Bridge Far Away...

ftgl is a c library which is requisite  
on everything but windows this is easy, most repos have it, on dogshit windows you will need to build in visual studio  
use `cabal new-build abfa` to download dependencies and compile  
use `./dist-newstyle/build/[arch]/ghc-[ver]/abfa-[ver]/x/abfa/build/abfa/abfa +RTS -s -M[x]m -N[n]` to run with x megabytes and n cores  
src/Game/Settings.hs has some variables that you can mess around with...
