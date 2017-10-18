----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock.FSK
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Some experiments for transmitting FSK signals.
-- TODO : clean up

{-# LANGUAGE RankNTypes #-}
module Hardware.SiClock.FSK
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Data.Ratio
import Data.Time

import Hardware.SiClock

clkOff :: Synth ()
clkOff = setCLKControl CLK_0 [CLK_off]

-- | A message in Baudot code. One start and 2 Stop bits
someMsgBaudot :: String
someMsgBaudot = concatMap addStartStop
  [ "10000","00001","00101","10000","00100","11011"
   ,"10111","10011","11111","00100","00011","11001","01110","00100"]
  where
     addStartStop :: String -> String
     addStartStop x  = "0" ++ reverse x ++ "11"

-- | 45 Baud speed.
symbolTime45 :: DiffTime
symbolTime45 = 0.022


-- | RTTY is basically Baudot code + fsk2.
rtty :: DiffTime -> Frequency -> String -> Synth ()
rtty symbolTime f msg = do
  liftIO $ print msg
  pllFreq  <- askMaxPLLFrequency
  xtalFreq <- askXtalFrequency
  let
    space  = f + 2125
    mark = f + 2295
    clkDivider = (round (pllFreq / mark) % 1)

    pllMarkConf = toDividerConf 0   ( mark * clkDivider / xtalFreq )
    pllSpaceConf = toDividerConf 0  ( space * clkDivider / xtalFreq )


    sendSymbol '1' = setDividerRaw PLL_A pllMarkConf
    sendSymbol '0' = setDividerRaw PLL_A pllSpaceConf
    sendSymbol _   = setDividerRaw PLL_A pllSpaceConf
                       
  setCLKDivider CLK_0 0 clkDivider
  setCLKControl CLK_0 [CLK_on,CLK_multi,CLK_DRV8,CLK_multiPLLA,CLK_integer]
  pllReset
  sendSymbol '1'
  now <- liftIO $ fmap utctDayTime getCurrentTime
  let times = [now+symbolTime, now + 2*symbolTime ..] 
  d <- forM (zip times msg) $ \(t,c) -> do
    delay <- liftIO $ waitUntil t
    sendSymbol c
    return delay
  liftIO $ print $ minimum d

  setCLKControl CLK_0 [CLK_off]

-- todo refactor & test
timedFrequencyHopping :: Frequency -> [(DiffTime,Frequency)] -> Synth ()
timedFrequencyHopping baseFrequency schedule = do
  pllFreq  <- askMaxPLLFrequency
  xtalFreq <- askXtalFrequency
  let
    startTimes = map fst schedule
    clkDivider = (floor (pllFreq / baseFrequency ) % 1)
    pllDividers
       = map (\(_,f) -> toDividerConf 0 $
                        (f + baseFrequency) * clkDivider / xtalFreq)
             schedule
    ((start_time,start_divider):plan) = zip startTimes pllDividers

  setCLKDivider CLK_0 0 clkDivider
  pllReset

  void $ liftIO $ waitUntil start_time
  setDividerRaw PLL_A start_divider
  setCLKControl CLK_0 [CLK_on,CLK_multi,CLK_DRV8,CLK_multiPLLA,CLK_integer]

  d <- forM plan $ \(t,divider) -> do
    delay <- liftIO $ waitUntil t
    setDividerRaw PLL_A divider
    return delay
  liftIO $ print $ minimum d

  setCLKControl CLK_0 [CLK_off]

waitUntil :: DiffTime -> IO Int
waitUntil time = do
  now <- fmap utctDayTime getCurrentTime
  let
    sleep :: Int
    sleep = truncate $ (time - now) * 1000000
  if sleep < 0
     then putStrLn "missed time slot"
     else threadDelay sleep
  return sleep
