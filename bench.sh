#!/bin/sh

echo "* Laplace 0    400x400 -N1 -qg"
/usr/bin/time repa-examples/dist/build/repa-laplace/repa-laplace 0    repa-examples/Laplace/data/pls-400x400.bmp out.bmp +RTS -N1 -qg

echo "* Laplace 1000 400x400 -N1 -qg"
/usr/bin/time repa-examples/dist/build/repa-laplace/repa-laplace 1000 repa-examples/Laplace/data/pls-400x400.bmp out.bmp +RTS -N1 -qg

echo "* Laplace 1000 400x400 -N4 -qg"
/usr/bin/time repa-examples/dist/build/repa-laplace/repa-laplace 1000 repa-examples/Laplace/data/pls-400x400.bmp out.bmp +RTS -N4 -qg

echo "* Laplace 1000 400x400 -N8 -qg"
/usr/bin/time repa-examples/dist/build/repa-laplace/repa-laplace 1000 repa-examples/Laplace/data/pls-400x400.bmp out.bmp +RTS -N8 -qg

echo "* MMult 1024x1024 -N1 -qg"
/usr/bin/time repa-examples/dist/build/repa-mmult/repa-mmult -random 1024 1024 -random 1024 1024 +RTS -N1 -qg

echo "* MMult 1024x1024 -N4 -qg"
/usr/bin/time repa-examples/dist/build/repa-mmult/repa-mmult -random 1024 1024 -random 1024 1024 +RTS -N4 -qg

echo "* MMult 1024x1024 -N8 -qg"
/usr/bin/time repa-examples/dist/build/repa-mmult/repa-mmult -random 1024 1024 -random 1024 1024 +RTS -N8 -qg

