{-
  Functional Programming -- Lab Assignment 1
  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}

import Test.QuickCheck

{-
  In this lab assignment, you will implement the well-known "power"
  function in two different new ways. The power function takes two arguments
  n and k and computes nk.
  Your implementation only has to work for non-negative k.
-}

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1

{-
  In order to calculate "power n k", for a given n and k,
  how many computing "steps" are being used?

    For power n k, k+1 computing steps are being used.
-}

steps :: Integer -> Integer
steps 0 = 1
steps n = 1 + steps(n - 1)

-- Part 2

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power1: negative exponent"
power1 n 0 = 1
power1 n k = product [n | _ <- [1..k]]

-- Part 3

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power2: negative exponent"
power2 n 0 = 1
power2 n k | odd k = n * power2 n (k - 1)
           | otherwise = power2 (n * n) (k `div` 2)

-- Part 4

-- 4.A
{-
  Test cases
  1 : n=5     k=4     : simple case for power and
                        power1 but "even" case for power2.
  2 : n=3     k=4     : same as 1 but n < k.
  3 : n=4     k=4     : n = k.
  4 : n=4     k=5     : odd case for power2.
  5 : n=10000 k=10000 : "big" numbers.
  6 : n=0     k=0     : 0^0 = 1.
  7 : n=0     k=1     : 0^1 = 0.
  8 : n=-5    k=2     : k is even and n < 0, result should be > 0.
  9 : n=-5    k=3     : k is odd and n <0, result should be > 0.
  10: n=5     k=-2    : k < 0 error case. TO-DO
-}
testCases = [(5,4), (3,4), (4,4), (4,5), (10000,10000),
             (0,0), (0,1), (-5,2), (-5,3)]

-- 4.B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power  n k == power1 n k &&
                  power1 n k == power2 n k &&
                  power  n k == n^k

-- 4.C
testPower :: Bool
testPower = and [ prop_powers a b | (a,b) <- testCases]

-- 4.D
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)
