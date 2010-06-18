# clear && hlint mandelbrot.hs && echo && ghc --make mandelbrot.hs && echo && time ./mandelbrot && echo

clear

echo " running hlint..."
hlint mandelbrot.hs

echo " generating image..."
time runghc mandelbrot.hs | /usr/local/bin/convert - test.png

echo " opening image..."
open test.png

echo " returning to code..."
open mandelbrot.hs
