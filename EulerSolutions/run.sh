clear

hlint .
echo

ghc --make Euler.hs && echo && time ./Euler && echo
