----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock.I2C
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- I2C related functions.

{-# Language BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Hardware.SiClock.I2C
(
  I2CLib.Device
 ,module Hardware.SiClock.I2C
)
where
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word

import System.Hardware.I2C as I2CLib

-- | Synth a is the monad for Si PPL synthesizers.
type SynthT env m a = ReaderT (env, I2CLib.Device) m a

-- | Run an IO action with an I2C device and an I2C address.
runI2CWith ::
     FilePath -> Addr -> (I2CLib.Device -> IO a) -> IO a
runI2CWith deviceFile i2cAddress action = do
  withDevice deviceFile
    $ \device -> do
      setSlaveAddr device i2cAddress
      action device


-- | Lift an IO action on an I2C device to Synth.
deviceIO :: MonadIO m => (Device -> IO a) -> SynthT env m a
deviceIO action = do
  device <- asks snd
  liftIO $ action device

-- | Dump the content of the IC registers. (For testing)
dumpRegisters :: MonadIO m => SynthT env m ()
dumpRegisters = deviceIO $ \device -> do
  print "register map"
  forM_ [0..180] $ \addr -> do
    val <- readI2CBlockData device addr 1
    print (addr,BS.head val)

writeByteData :: MonadIO m => Command -> Word8 -> SynthT env m ()
writeByteData addr val
  = deviceIO $ \device -> I2CLib.writeByteData device addr val

writeI2CBlockData :: MonadIO m => Command -> ByteString -> SynthT env m ()
writeI2CBlockData addr bs
  = deviceIO $ \device -> I2CLib.writeI2CBlockData device addr bs

