----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock.MorseKeyer
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Sending Morse code.
-- Don't use it for real (hard key clicks).

{-# LANGUAGE RankNTypes #-}
module Hardware.SiClock.MorseKeyer
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

import Hardware.SiClock

-- | Quick test.
test :: IO ()
test = runSynth $ sendMsg someFreq someMsg

-- | Some Message in Morse Code.
someMsg :: String
someMsg ="-.-. --.-   -.-. --.-   ... .. ..... ...-- ..... .----   - . ... -   .- -... -.-. -.." -- cq cq si5351 test

-- | Some frequency for testing (10m Band)
someFreq :: Frequency
someFreq = 28004452

-- | Main time unit in mico-seconds.
ditLen :: Int
ditLen = 60*1000

-- | Send a message.
sendMsg :: Frequency -> String -> Synth ()
sendMsg freq msg = do
  void $ setDividers PLL_A CLK_0 freq
  pllReset
  forM_ msg sendSym

-- | Send a symbol. (either \'.\', \'-\' or \' \')
sendSym :: Char -> Synth ()
sendSym sym = case sym of
  '.' -> do rfOn >> ditDelay >> rfOff >> ditDelay    
  '-' -> do rfOn >> daDelay  >> rfOff >> ditDelay
  ' ' -> do ditDelay >> ditDelay
  other -> liftIO $ print ("ignoring Symbol",other)

ditDelay :: Synth ()
ditDelay = liftIO $ threadDelay ditLen

daDelay :: Synth ()
daDelay = ditDelay >> ditDelay >> ditDelay

-- | Hard key on.
rfOn :: Synth ()
rfOn = clk0_On

-- | Hard key off.
rfOff :: Synth ()
rfOff = clk0_Off
