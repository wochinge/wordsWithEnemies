-- | Offers missing Math methods.
module Utils.MathUtil
( fac
) where

-- | Faculty method to calculate the possible permutations.
fac :: (Enum a, Num a) 
    => a -- ^ number
    -> a -- ^ faculty of the number.
fac n = product [n, n-1 .. 1]