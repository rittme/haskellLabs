-- From the lecture

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1 
-- Number of steps  for power n k ==> k + 1
-- steps n k | k < 0 = error "steps: negative argument"
-- steps n 0 = 1
-- steps n k = 1 + steps n (k-1)


-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = (product prod) 
	where prod = replicate (fromInteger k) (fromInteger n)
--power1 n k = product listNumbers 
		--where listNumbers = [(x `div` x) *n | x <- [1..k]]


-- Part 3 
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k  | even k = (n * n)^halfK
	where halfK = (k `div` 2)
power2 n k  | otherwise = n * (n^(k - 1))

-- Part 4

-- A 

-- B
prop_power n k = let k' = abs k in
                  (power n k' == power1 n k')
                  && (power n k' == power2 n k')
                  && (power n k' == n^k')
