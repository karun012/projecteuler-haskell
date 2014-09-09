module PalindromeProduct (
    factors
) where

data PalindromeProductFactors = PalindromeProductFactors {
    lhs :: Integer,
    rhs :: Integer,
    product :: Integer
} deriving (Show)

-- | Is palindrome
--
-- >>> isPalindrome 9009
-- True
--
-- >>> isPalindrome 1234
-- False
--
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- | Generates all combinations of values from two universes
--
-- >>> tuples [1,2] [3,4]
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- >>> tuples ['a'] ['b','c']
-- [('a','b'),('a','c')]
--
tuples :: [x] -> [y] -> [(x,y)]
tuples xs ys = [(x, y) | x <- xs, y <- ys]

-- | Tells if the product of the tuple is a palindrome
--
-- >>> isProductPalindrome (91,99)
-- True
--
-- >>> isProductPalindrome (2,8)
-- False
--
isProductPalindrome :: (Integer,Integer) -> Bool
isProductPalindrome (lhs,rhs)= isPalindrome (lhs * rhs)

-- | Palindrome product factors
--
-- >>> palindromeProductFactors [91,80,8] [99,81,2]
-- [(91,99)]
--
palindromeProductFactors :: [Integer] -> [Integer] -> [(Integer,Integer)]
palindromeProductFactors xs ys = filter isProductPalindrome $ tuples xs ys

-- | Factors of the largest palindrome product
--
-- >>> largestProductFactors [10..99] [10..99]
-- (99,91)
--
-- >>> largestProductFactors [2] [8]
-- (0,0)
--
largestProductFactors :: [Integer] -> [Integer] -> (Integer,Integer)
largestProductFactors xs ys = foldr (\(x1,y1) (x2,y2) -> if (x1 * y1) > (x2 * y2) then (x1, y1) else (x2, y2)) (0,0) (palindromeProductFactors xs ys)

-- | Factors of the largest number that is a palindrome. This is almost similar to the above function. It just wraps the result in a custom datatype
--
-- >>> factors [10..99] [10..99]
-- Just (PalindromeProductFactors {lhs = 99, rhs = 91, product = 9009})
--
-- >>> factors [2] [8]
-- Nothing
--
factors :: [Integer] -> [Integer] -> Maybe PalindromeProductFactors
factors xs ys = case largestProductFactors xs ys of
                    (0,0) -> Nothing
                    (lhs,rhs) -> Just (PalindromeProductFactors lhs rhs (lhs * rhs))
