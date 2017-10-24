----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock.Divider
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module contains utility functions for fractional PLL divers.
-- Most arduino Si5351 packages use dividers with a fixed large denominator,
-- which seens to be against the spirit of the Si5351 design.
-- I use continued fractions to compute the best fractional approximation.
-- (This may be an overkill but why not.)

module Hardware.SiClock.Divider
where
  
import Data.Word
import Data.Ratio
import qualified Data.List

-- | Compute the pvalues for a rational divider.
-- Any denominator is pemissible here.
-- The function uses the best approximation.

dividerToPVal :: Rational -> (Word32, Word32, Word32)
dividerToPVal d = (fromInteger p1,fromInteger p2,fromInteger c)
  where
    (a,b,c) = dividerToABC d
    fl = ((128*b) `div` c)
    p1 = 128 * a + fl - 512
    p2 = 128 * b - c * fl

-- | Approximate the a,b,c values of a divider.
dividerToABC :: Rational ->  (Integer, Integer, Integer)
dividerToABC divider = (a, b, c)
  where
    d = denominator divider
    n = numerator divider
    a = n `div` d
    rest = (n `mod` d) % d 
    fraction = last $ filter (\r -> (denominator r < 0x100000))
                 $ approximations rest
    b = numerator fraction
    c = denominator fraction

-- | Compute a list of approximations of a rational number.
approximations :: Rational -> [Rational]
approximations r
  = map (\l -> fromContinuedFraction (n,l)) $ Data.List.inits cf
  where    
    (n,cf) = toContinuedFraction r

-- | continued fraction stuff.
toContinuedFraction :: Rational -> (Integer,[Integer])
toContinuedFraction r = (n `div` d , cf (n `mod` d) d)
  where
    n = numerator r
    d = denominator r 
    cf 0 _ = []
    cf a b = (b `div` a) : cf (b `mod` a) a

-- | continued fraction stuff.
fromContinuedFraction :: (Integer,[Integer]) -> Rational
fromContinuedFraction (n,cf ) = fromInteger n + toRat cf
  where
    toRat :: [Integer] -> Rational
    toRat [] = 0 
    toRat (h:r) = 1 / ( fromInteger h + toRat r)                      
