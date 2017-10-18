----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock.Examples
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module collects some examples and tests.
-- Unless stated otherwise the output pin is CLK_0.

{-# LANGUAGE RankNTypes #-}
module Hardware.SiClock.Examples where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

import Hardware.SiClock
import Hardware.SiClock.FSK as FSK
import Hardware.SiClock.MorseKeyer as MorseKeyer
import Hardware.SiClock.JT65Test (jt65SendHelloWorld)


-- | Set Clk_0 to frequency f and turn on clock output.
testSynth :: Frequency -> IO ()
testSynth f = runSynth $ do
    liftIO $ putStrLn $ "setting clk0 to " ++ show f
    void $ setDividers PLL_A CLK_0 f
    pllReset
    clk0_On
    liftIO $ putStrLn $ "clk0_on "

-- | Turn off CLK_0.
clkOff :: IO ()
clkOff = runSynth $ setCLKControl CLK_0 [CLK_off]

-- | Read the chip registers.
testI2CReading :: IO ()
testI2CReading = runSynth dumpRegisters

someHopFrequencies :: [Frequency]
someHopFrequencies =  [28120000,28120100..28120400]

-- | One example for frequency hopping.
-- Here the PLL stays at one frequency and the clock divider is modified.
testHopping :: [Frequency] -> IO ()
testHopping sw = runSynth $ do
  maxPLLFreq  <- askMaxPLLFrequency
  xtalFreq <- askXtalFrequency
  setPLLDivider_A $ maxPLLFreq / xtalFreq
  forever $ forM_ sw $ \freq -> do
      setCLKDivider CLK_0 0 $ maxPLLFreq / freq
      pllReset
      clk0_On
      liftIO $ threadDelay 1500000

-- | Send a JT65 hello world message.
testJT65 :: Frequency -> IO ()
testJT65 f = runSynth $ jt65SendHelloWorld f

-- | Send some message in morse code.
testMorse :: Frequency -> IO ()
testMorse f
  = runSynth $ MorseKeyer.sendMsg f MorseKeyer.someMsg

-- | Send some RTTY message
testRTTY :: Frequency -> IO ()
testRTTY f
  = runSynth $ FSK.rtty FSK.symbolTime45 f FSK.someMsgBaudot

-- | Switching the PLL causes some loud clicks (unwanted).
testClicks1 :: Frequency -> IO ()
testClicks1 f = runSynth $ do
    maxPLLFreq  <- askMaxPLLFrequency
    xtalFreq <- askXtalFrequency
    setPLLDivider_A $ maxPLLFreq / xtalFreq
    setPLLDivider_B $ maxPLLFreq / xtalFreq
    setCLKDivider CLK_0 0 $ maxPLLFreq / f
    pllReset
    forever $ do
      liftIO $ putStrLn "A"
      setCLKControl CLK_0 [CLK_on,CLK_multi,CLK_DRV8,CLK_multiPLLA]
      liftIO $ threadDelay $ 2000*1000
      liftIO $ putStrLn "B"
      setCLKControl CLK_0 [CLK_on,CLK_multi,CLK_DRV8,CLK_multiPLLB]
      -- only clicks here ??
      -- something wrong with PLL_B maybe not initialized
      liftIO $ threadDelay $ 2000*1000

-- | No clicks here. I don't know why ?
noClicks2 :: Frequency -> IO ()
noClicks2 f = runSynth $ do
  maxPLLFreq  <- askMaxPLLFrequency
  xtalFreq <- askXtalFrequency
  setPLLDivider_A $ maxPLLFreq / xtalFreq
  setCLKDivider CLK_0 0 $ maxPLLFreq / f

  forever $ do
      setPLLDivider_A $ maxPLLFreq / xtalFreq
      setCLKDivider CLK_0 0 $ maxPLLFreq / f
      setCLKControl CLK_0 [CLK_on,CLK_multi,CLK_DRV8,CLK_multiPLLA]
  --    pllReset
      liftIO $ threadDelay $ 500*1000
