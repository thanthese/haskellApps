# Stephen Mann
# August 24, 2008
# Solution 12
#
# The problem:
#
#   The sequence of triangle numbers is generated by adding the natural
#   numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 =
#   28. The first ten terms would be:
#
#   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
#
#   Let us list the factors of the first seven triangle numbers:
#
#	  1: 1
#	  3: 1,3
#	  6: 1,2,3,6
#	 10: 1,2,5,10
#	 15: 1,3,5,15
#	 21: 1,3,7,21
#	 28: 1,2,4,7,14,28
#
#   We can see that 28 is the first triangle number to have over five
#   divisors.
#
#   What is the value of the first triangle number to have over five hundred
#   divisors?
#
# I was having some difficulty with this problem until I realized two things.
#
#  (1) You only need to loop as high as the square root of the number when
#   checking for divisors.
#
#  (2) The problem asked for GREATER than 500, not 500 on the nose.  That makes
#   finding the answer much less like a search for a needle in a haystack.
#
import math
numOfDivisiorsGoal = 500
numOfTriangleNumbers = 100000
# functions
def numOfDivisors(theBigNumber):
   total = 2 # already counting 1 and itsel
   for i in range(2, int(math.sqrt(theBigNumber)) + 1):
	 if theBigNumber % i == 0:
	   if theBigNumber / i == i:
		 total += 1
	   else:
		 total += 2
   return total
# create list of triangle numbers
triangleNumbers = []
runningTotal = 0
for i in range(numOfTriangleNumbers):
   runningTotal += i
   triangleNumbers.append(runningTotal)
# hunt for the first
for i in range(0, len(triangleNumbers)):
   temp = numOfDivisors(triangleNumbers[i])
   if temp >= numOfDivisiorsGoal:
	 print "%d is a triangle number with %d divisors" %(triangleNumbers[i], temp)
	 break


