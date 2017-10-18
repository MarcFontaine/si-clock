----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock.JT65Test
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This Module contains an example for transmitting a JT65 message.
-- The message is a hardcoded 'hello world'.
-- This Module does NOT contain an implementation of the JT65 codec.
-- (The codec is in the jt65Codec package.)

{-# LANGUAGE RankNTypes #-}
module Hardware.SiClock.JT65Test
where
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Data.Time
import Data.Word
import Data.Ratio

import Hardware.SiClock
import Hardware.SiClock.FSK

-- | Send a JT65 'hello world' on a given frequency.
-- The transmission starts at beginning of the next full minute. 
jt65SendHelloWorld :: Frequency -> Synth ()
jt65SendHelloWorld baseFrequency = do
  now <- liftIO $ fmap utctDayTime getCurrentTime 
  liftIO $ do
    putStrLn $ "jt65 sending Hello World at " ++ show baseFrequency ++ " Hz"
    putStrLn "transmission starts at full minute"
    print now
  let
      startTime :: Integer
      startTime = (ceiling ((now + 2 ) / 60) * 60)
      symbolTimes = map ((+) (fromIntegral startTime)) jt65SymbolStartTimes
  do
      timedFrequencyHopping baseFrequency
         (zip symbolTimes hello_world_frequencies)
      liftIO $ threadDelay $ 400 * 1000
      setCLKControl CLK_0 [CLK_off]
  liftIO $ putStrLn "done"


-- | The list of symbols of the 'hello world' message.
-- (acutally not needed.)
hello_world_symbols :: [Word8]
hello_world_symbols
 = [ 29,23,60,48,34,6,39,9,23,26,55,15,47,12,16,42,11,25,63,63,9
   , 10,60,0,46,21,15,54,54,62,51,48,39,20,56,25,15,62,52,36,3,4
   , 41,13,59,10,41,63,43,39,15,19,32,33,53,25,60,62,4,55,26,42,48]

-- | The list of pre computed frequencies for the 'hello world' message.
hello_world_frequencies :: [Frequency]
hello_world_frequencies =
  [2541 % 2,13539427 % 10000,535117 % 400,2541 % 2,2541 % 2,7186927 % 5000
  ,281017 % 200,3418503 % 2500,2541 % 2,2541 % 2,2541 % 2,2541 % 2,2541 % 2
  ,2541 % 2,807521 % 625,2541 % 2,13808597 % 10000,2541 % 2,13001087 % 10000
  ,535117 % 400,3364669 % 2500,2541 % 2,14239269 % 10000,2541 % 2,2541 % 2
  ,13162589 % 10000,14023933 % 10000,2541 % 2,6540919 % 5000,6594753 % 5000
  ,3472337 % 2500,2541 % 2,2541 % 2,2541 % 2,13054921 % 10000,13431759 % 10000
  ,2541 % 2,2541 % 2,2541 % 2,2541 % 2,2890921 % 2000,2541 % 2,2541 % 2
  ,2890921 % 2000,2541 % 2,2541 % 2,2541 % 2,2541 % 2,13001087 % 10000
  ,3257001 % 2500,7186927 % 5000,2541 % 2,2541 % 2,6379417 % 5000,2541 % 2
  ,1749627 % 1250,2541 % 2,13324091 % 10000,2541 % 2,2541 % 2,13162589 % 10000
  ,888272 % 625,2541 % 2,2541 % 2,888272 % 625,2541 % 2,1803461 % 1250,2541 % 2
  ,14131601 % 10000,2541 % 2,281017 % 200,13808597 % 10000,2541 % 2,6648587 % 5000
  ,7133093 % 5000,13431759 % 10000,13162589 % 10000,1803461 % 1250,7079259 % 5000
  ,2541 % 2,2541 % 2,6863923 % 5000,2567917 % 2000,6433251 % 5000,13862431 % 10000
  ,2621751 % 2000,14346937 % 10000,3257001 % 2500,2541 % 2,2541 % 2
  ,13862431 % 10000,2541 % 2,2890921 % 2000,2783253 % 2000,2541 % 2
  ,13808597 % 10000,2541 % 2,2541 % 2,13162589 % 10000,2541 % 2,13270257 % 10000
  ,2541 % 2,6810089 % 5000,2541 % 2,2729419 % 2000,2837087 % 2000,2541 % 2
  ,2541 % 2,13431759 % 10000,7186927 % 5000,2541 % 2,1803461 % 1250,6433251 % 5000
  ,2541 % 2,14239269 % 10000,3364669 % 2500,3472337 % 2500,281017 % 200,2541 % 2
  ,2541 % 2,2541 % 2,2541 % 2,2541 % 2,2541 % 2,2541 % 2,2541 % 2
  ]

-- | Symbol start times relative to the full minute.
jt65SymbolStartTimes :: [DiffTime]
jt65SymbolStartTimes
  = take 126 $ map fromRational [0 , (4096 % 11025) ..]

