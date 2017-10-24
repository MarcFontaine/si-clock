----------------------------------------------------------------------------
-- |
-- Module      :  Hardware.SiClock
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This is the main API.

{-# Language BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Hardware.SiClock
(
  module Hardware.SiClock
 ,module I2C
)

where
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Ratio
import Text.Read (readMaybe)
import GHC.Stack
import System.Environment (lookupEnv)
  
import Hardware.SiClock.I2C as I2C
  (SynthT,runI2CWith,writeByteData,writeI2CBlockData,dumpRegisters)
import Hardware.SiClock.Divider

type Frequency = Rational
type Divider = Rational

data Config = Config {
   _I2CDevice :: FilePath
  ,_I2CAddress  :: Word8
  ,_XtalFrequency   :: Frequency
  ,_maxPLLFrequency :: Frequency
  } deriving (Show,Eq)

-- | The defaultConfig if no environment variables are set.
defaultConfig :: Config
defaultConfig = Config {
   _I2CDevice       = "/dev/i2c-7"
  ,_I2CAddress      = 0x60
  ,_XtalFrequency   =  27000000
  ,_maxPLLFrequency = 700000000
  }                    

-- | Check that defaultConfigEnv matches your hardware before you use it.
-- Do not run any SiPLL code on a wrong i2c-bus,
-- i.e. an internal I2C bus of your PC.
-- (it might confuse and or wreck) your hardware.
-- You can overwrite config values with the following ENV variables:
-- SI_CLOCK_I2C_DEVICE
-- SI_CLOCK_I2C_ADDRESS
-- SI_CLOCK_XTAL_FREQUENCY
-- SI_CLOCK_MAX_PLL_FREQUENCY

defaultConfigEnv :: HasCallStack => IO Config
defaultConfigEnv = do
  device <- lookupEnv _SI_CLOCK_I2C_DEVICE
  when (device == Nothing)
    $ error "Check documentation! For example setenv SI_CLOCK_I2C_DEVICE=/dev/i2c-7." 
  let (Just _I2CDevice) = device
  addr <- lookupEnv _SI_CLOCK_I2C_ADDRESS
  _I2CAddress <- case addr of
     Just str -> readSafe str
     Nothing    -> return $ _I2CAddress defaultConfig
  _XtalFrequency   <- readFractional _SI_CLOCK_XTAL_FREQUENCY _XtalFrequency
  _maxPLLFrequency <- readFractional _SI_CLOCK_MAX_PLL_FREQUENCY _maxPLLFrequency
  return $ Config {..}
  where
    readFractional :: String -> (Config -> Rational) -> IO Rational
    readFractional envName field = do
      e <- lookupEnv envName
      case e of
        Nothing -> return $ field defaultConfig
        Just str -> do
          r <- readSafe str
          return (r %1)
    readSafe :: Read a => String -> IO a
    readSafe str = maybe (error $ show ("no parse",str)) 
                         return (readMaybe str)
-- | Quick test for the I2C connection (with default config).
testIO :: IO ()
testIO = runSynth dumpRegisters

type Synth a = forall m. MonadIO m => SynthT Config m a

-- | Run the Synth monad with the config from defaultConfigEnv.
-- | .i.e. reading Env
runSynth :: HasCallStack => SynthT Config IO a -> IO a
runSynth action = do
  config <- defaultConfigEnv
  runSynthWith config action

-- | Run the Synth monad with a custom configuration.
runSynthWith :: HasCallStack => Config -> SynthT Config IO a -> IO a
runSynthWith conf action
  = runI2CWith
      (_I2CDevice conf)
      (_I2CAddress conf)
      (\device -> runReaderT action (conf,device))

askXtalFrequency :: Synth Frequency
askXtalFrequency = asks ( _XtalFrequency . fst)

askMaxPLLFrequency :: Synth Frequency
askMaxPLLFrequency = asks ( _maxPLLFrequency . fst)

-- | An IC has PLL_A and PLL_B.
data PLL = PLL_A | PLL_B deriving (Show,Eq)

-- | An IC has up to 8 clocks. (CLK_0..CLK_7).
data CLK = CLK_0 | CLK_1 | CLK_2 | CLK_3 | CLK_4 | CLK_5 | CLK_6 | CLK_7
  deriving (Show,Eq,Ord,Enum)

-- | Reset (both?) PLLs
pllReset :: Synth ()
pllReset = writeByteData _SI_PLL_RESET 0xA0

-- | Turn on CLK_0 output.
clk0_On :: Synth ()
clk0_On = writeByteData _SI_CLK0_CONTROL 0x4f

-- | Turn off CLK_0 output.
clk0_Off :: Synth ()
clk0_Off = writeByteData _SI_CLK0_CONTROL 0x00

data DividerPair
 = DividerPair {_pllDivider :: Divider,_clkDivider :: Divider}
 deriving Show
          
-- | Set PLL and Clock dividers for a frequency.
setDividers :: PLL -> CLK -> Frequency -> Synth DividerPair
setDividers pll clk f = do
  dividers <- defaultDividers f
  setPLLDivider pll   $ _pllDivider dividers
  setCLKDivider clk 0 $ _clkDivider dividers
  return dividers

-- | Compute a pair of good default pll and clk dividers.
-- (clk divider is an integer)
defaultDividers :: Frequency -> Synth DividerPair
defaultDividers f = do
  pllFrequency  <- askMaxPLLFrequency
  xtalFrequency <- askXtalFrequency
  let 
    clkDivider = (floor (pllFrequency / f ) % 1)
    pllDivider = f * clkDivider / xtalFrequency
  return $ DividerPair {_pllDivider =pllDivider, _clkDivider =clkDivider}
  
-- | Set a PLL fractional divider
setPLLDivider :: PLL -> Divider -> Synth ()
setPLLDivider p divider
  = setDividerRaw p $ toDividerConf 0 divider

-- | Short for setPLLDivider PLL_A
setPLLDivider_A :: Divider -> Synth ()
setPLLDivider_A = setPLLDivider PLL_A

-- | Short for setPLLDivider PLL_B
setPLLDivider_B :: Divider -> Synth ()
setPLLDivider_B = setPLLDivider PLL_B

-- | Setup a Clock divider.
-- The rfield is passed as a plain Word8.
-- (ToDo high level API for rfields).
setCLKDivider :: CLK -> Word8 -> Divider -> Synth ()
setCLKDivider clk rfield divider
  = setDividerRaw clk $ toDividerConf rfield divider


-- | Bits in the clock control registers.
data CLK_Control_bits
  = CLK_on
  | CLK_off
  | CLK_fractional
  | CLK_integer
  | CLK_multiPLLA
  | CLK_multiPLLB
  | CLK_inverted
  | CLK_XTAL
  | CLK_CLKin
  | CLK_multi
  | CLK_DRV2
  | CLK_DRV4
  | CLK_DRV6
  | CLK_DRV8
  deriving (Show,Eq)

setCLKControl :: CLK -> [CLK_Control_bits] -> Synth ()
setCLKControl clk bits
  = setCLKControlRaw clk $ controlBitsToWord8 bits

setCLKControlRaw :: CLK -> Word8 -> Synth ()
setCLKControlRaw clk bits = writeByteData addr bits
  where
    addr = case clk of
      CLK_0 -> 16
      CLK_1 -> 17
      CLK_2 -> 18
      CLK_3 -> 19
      CLK_4 -> 20
      CLK_5 -> 21     
      CLK_6 -> 22
      CLK_7 -> 23

controlBitsToWord8 :: [CLK_Control_bits]  -> Word8
controlBitsToWord8 bits
  = foldl (\b f ->b .|. bitOf f) 0 bits
  where
    bitOf f = case f of
      CLK_on           -> 0x00
      CLK_off          -> 0x80
      CLK_fractional   -> 0x00
      CLK_integer      -> 0x40
      CLK_multiPLLA    -> 0x00
      CLK_multiPLLB    -> 0x20
      CLK_inverted     -> 0x10
      CLK_XTAL         -> 0x00
      CLK_CLKin        -> 0x08
      CLK_multi        -> 0x0c
      CLK_DRV2         -> 0x00
      CLK_DRV4         -> 0x01
      CLK_DRV6         -> 0x02
      CLK_DRV8         -> 0x03
      
-- | A DividerConf is basically  the bytestring that configures a fractional divider.
newtype DividerConf = DividerConf {unDividerConf :: ByteString}
  deriving (Show,Eq)

-- | Mangle a Divider and a rval into a DividerConf.
-- This can be used to pre-compute all the math and to get the bits
-- that define a divider.
toDividerConf :: Word8 -> Divider -> DividerConf
toDividerConf rval divider  = DividerConf $ BS.pack [
     pt  8 p3
  ,  pt  0 p3
  ,((pt 16 p1) .&. 3) .|. rval
  ,  pt  8 p1
  ,  pt  0 p1
  ,((pt 12 p3) .&. 0xf0) .|. ((pt 16 p2) .&. 0x0f)  
  ,  pt  8 p2
  ,  pt  0 p2
  ]
  where
    (p1,p2,p3) = dividerToPVal divider
    pt :: Int -> Word32 -> Word8
    pt s w = fromIntegral $ ((w `rotateR` s) .&. 0xff)

-- | Setup some fractional divider with a pre-computed config.
-- Using a pre-computed config might be faster or more convenient.
setDividerRaw :: DividerAddr hw => hw -> DividerConf -> Synth ()
setDividerRaw hw (DividerConf bs)
  = writeI2CBlockData (toDividerAddr hw) bs

-- | Get address of the fractional divider.
class DividerAddr a where
  toDividerAddr :: a -> Word8

-- | Address of a PLL divider.
instance DividerAddr PLL where
  toDividerAddr PLL_A = _SI_SYNTH_PLL_A
  toDividerAddr PLL_B = _SI_SYNTH_PLL_B

-- | Address of a Clock divider.
instance DividerAddr CLK where
  toDividerAddr clk = case clk of
    CLK_0 -> 42
    CLK_1 -> 50
    CLK_2 -> 58
    CLK_3 -> 66
    CLK_4 -> 74
    CLK_5 -> 82     
    CLK_6 -> undefined
    CLK_7 -> undefined

-- | Generic Address of fractional divider.
instance DividerAddr Word8 where
  toDividerAddr = id

_SI_CLK0_CONTROL :: Word8
_SI_CLK0_CONTROL = 16
_SI_CLK1_CONTROL :: Word8
_SI_CLK1_CONTROL = 17
_SI_CLK2_CONTROL :: Word8
_SI_CLK2_CONTROL = 18
_SI_SYNTH_PLL_A  :: Word8
_SI_SYNTH_PLL_A  = 26
_SI_SYNTH_PLL_B  :: Word8
_SI_SYNTH_PLL_B  = 34
_SI_PLL_RESET    :: Word8
_SI_PLL_RESET    = 177

_SI_CLOCK_I2C_DEVICE :: String
_SI_CLOCK_I2C_DEVICE = "SI_CLOCK_I2C_DEVICE"
_SI_CLOCK_I2C_ADDRESS :: String
_SI_CLOCK_I2C_ADDRESS = "SI_CLOCK_I2C_ADDRESS"
_SI_CLOCK_XTAL_FREQUENCY :: String
_SI_CLOCK_XTAL_FREQUENCY = "SI_CLOCK_XTAL_FREQUENCY"
_SI_CLOCK_MAX_PLL_FREQUENCY :: String
_SI_CLOCK_MAX_PLL_FREQUENCY ="SI_CLOCK_MAX_PLL_FREQUENCY"
