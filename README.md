# A Bridge Far Away...

ftgl is a c library which is requisite  
on everything but windows this is easy, most repos have it, on dogshit windows you will need to build in visual studio  

`cabal build abfa` is now the only way to build this project, new-build will bug, `./dist/build/abfa/abfa +RTS -s -M[x]m -N[n]` is the way to run it.  i leave the following so i remember how to do it when new-cabal fixes its bugs...

use `cabal new-build abfa` to download dependencies and compile  
use `./dist-newstyle/build/[arch]/ghc-[ver]/abfa-[ver]/x/abfa/build/abfa/abfa +RTS -s -M[x]m -N[n]` to run with x megabytes and n cores  
src/Game/Settings.hs has some variables that you can mess around with...
