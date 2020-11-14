# A Bridge Far Away...

use `cabal new-build abfa` to download dependencies and compile

use `./dist-newstyle/build/[arch]/ghc-[ver]/abfa-[ver]/x/abfa/opt/build/abfa/abfa +RTS -s -M[x]m -N[n]` to run with x megabytes and n cores  

performance is increased drastically by disabling the development flag, but compilation takes forever

to create profiling files, use `cabal new-build --enable-library-profiling --enable-profiling abfa` to build, and `-prof -fprof-auto` in the ghc-options of the cabal file.  run with flags `+RTS -s -p -hy -M[x]m -N[n]`
