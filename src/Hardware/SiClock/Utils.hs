module Hardware.SiClock.Utils
(
   module Hardware.SiClock.Utils
  ,setEnv
)
where

import Data.Ratio
import System.Environment (setEnv)

import Hardware.SiClock

-- | Sample an interval from start to end with steps.
fromToSteps :: Rational -> Rational -> Integer -> [Rational]
fromToSteps start end steps
  = [ start + (end - start) * (i % steps) | i <-[0..steps-1]]

-- | Set the SI_CLOCK_I2C_DEVICE environment variable.
-- Useful with ghci.
setEnvI2CDevice :: String -> IO ()
setEnvI2CDevice = setEnv _SI_CLOCK_I2C_DEVICE

-- | Set the SI_CLOCK_XTAL_FREQUENCY environment variable.
-- Useful with ghci.
setEnvXtalFrequency :: Integer -> IO ()
setEnvXtalFrequency f = setEnv _SI_CLOCK_XTAL_FREQUENCY $ show f
