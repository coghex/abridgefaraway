# A Bridge Far Away...

A Bridge Far Away... is a game engine written in haskell, using vulkan and glfw.  the code structure is devided up into many parts arbitrarily named.  paracletus is the graphics engine, anamnesis is the program structure and continuation monad, and epiklesis is the lua interpreter.  Works on windows, linux, or mac, probably anything with GHC and vulkan.

the mod directory contains examples of lua files for how to define windows, UI elements, and general game structures.  all these commands could also be run in game from the shell. mod/base/config.lua contains the settings for key layout and such.

use `cabal new-build abfa` to download dependencies and compile

use `./dist-newstyle/build/[arch]/ghc-[ver]/abfa-[ver]/x/abfa/opt/build/abfa/abfa +RTS -s -M[x]m -N[n]` to run with x megabytes and n cores  

performance is increased drastically by disabling the development flag, but compilation takes forever

to create profiling files, use `cabal new-build --enable-library-profiling --enable-profiling abfa` to build, and `-prof -fprof-auto` in the ghc-options of the cabal file.  run with flags `+RTS -s -p -hy -M[x]m -N[n]`
