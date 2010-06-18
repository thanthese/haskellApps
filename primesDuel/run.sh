clear

hlint .
echo 

ghc --make stolenPrime.hs
ghc --make eulerSievePrimes.hs

echo
echo "*** run stolenPrime"
time ./stolenPrime

echo
echo "*** run eulerSievePrimes (mine)"
time ./eulerSievePrimes
