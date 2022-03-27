{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Clash.Privy.Internal where

import Control.Exception
import System.IO.Unsafe
import           Data.Typeable
import           Data.Dynamic
import Control.Lens
import Data.Semigroup
import Data.IORef
import GHC.Stack

import qualified Data.HashMap.Strict    as HM

-- privily

-- | Privy is a way of transporting data behind the scenes in such a way that Clash will ignore it
-- when compiling to RTL but it will be available for simulations using GHC.
--
-- Privys are heterogeneous containers indexed by the type of the item stored. Items with the same
-- time will be combined with `(<>)` when Privys are combined. The idea is that there is one Privy
-- for a design
--
-- Internally this is done by using exceptions to carry the data. The internal data is stored with
-- a TypeRep HashMap so you can store anything you like inside.
data Privy = PrivyBox PrivyInternal

instance Show Privy where
  show = const "Privy"

-- | Use a wrapper for where the exception is actually thrown from so that running `seq` on `Privy`
-- doesn't throw an exception.
data PrivyInternal

purePrivy :: PrivyValues -> Privy
purePrivy = PrivyBox . unsafePerformIO . throw

takePV :: Privy -> PrivyValues
takePV (PrivyBox p) = unsafePerformIO $ do
  Left pv <- try (pure $! p)
  pure pv

mapPV :: (PrivyValues -> PrivyValues) -> Privy -> Privy
mapPV f (PrivyBox p) = PrivyBox $ unsafePerformIO $ do
  Left pv <- try (pure $! p)
  throw $! f pv

data PV where
  PV :: (Typeable a, Semigroup a) => a -> PV

fromPV :: Typeable a => PV -> Maybe a
fromPV (PV a) = cast a

mapPV2 :: (PrivyValues -> PrivyValues -> PrivyValues) -> Privy -> Privy -> Privy
mapPV2 f (PrivyBox p1) (PrivyBox p2) = PrivyBox $ unsafePerformIO $ do
  Left pv1 <- try (pure $! p1)
  Left pv2 <- try (pure $! p2)
  throw $! f pv1 pv2

instance Semigroup Privy where
  (<>) = mapPV2 (\(PVs as) (PVs bs) -> PVs $ HM.unionWith (\(PV a) (PV b) -> case cast b of Just b' -> PV (a <> b')) as bs)

instance Monoid Privy where
  mempty = purePrivy (PVs mempty)

instance Exception PrivyValues

newtype PrivyValues = PVs (HM.HashMap TypeRep PV)

instance Show PrivyValues where
  show = const "PrivyValues"

getterRep :: forall a s. Typeable s => Getting a s a -> TypeRep
getterRep _ = typeRep (Proxy :: Proxy s)

lookupPrivyVal :: Typeable a => PrivyValues -> Maybe a
lookupPrivyVal = lookupPrivyValWith id

lookupPrivyValWith :: Typeable a => Getting r a r -> PrivyValues -> Maybe r
lookupPrivyValWith g (PVs hm) = HM.lookup (getterRep g) hm >>= fromPV <&> view g

takePrivyVal :: Typeable a => PrivyValues -> Maybe (PrivyValues, a)
takePrivyVal = takePrivyValWith id

takePrivyValWith :: Typeable a => Getting r a r -> PrivyValues -> Maybe (PrivyValues, r)
takePrivyValWith g (PVs hm) = HM.lookup rep hm >>= fromPV <&> \v -> (PVs (HM.delete rep hm), view g v)
  where rep = getterRep g

takePrivyValue :: Typeable a => Privy -> Maybe (Privy, a)
takePrivyValue pv = takePrivyVal (takePV pv) <&> \(pv', a) -> (purePrivy pv', a)

intPrivy :: Sum Int -> Privy
intPrivy i = purePrivy $ PVs (HM.singleton (typeOf i) (PV i))

-- | A Wormhole is a way of transporting data from another location. The data needs to be filled in
-- before the value is used (otherwise an error is thrown).
--
-- It can be useful to have a Wormhole hidden in a Privy that can be filled in before running a
-- simulation, similar to a bind state in verilog.
data Wormhole a = Wormhole (IORef (Bool, a))

-- | Create a new wormhole. @a@ must not be looked at until the wormhole has been filled in,
-- otherwise an error will be thrown.
newWormhole :: HasCallStack => (Wormhole a, a)
newWormhole = unsafePerformIO $ do
  w <- newIORef (False, error "unfilled wormhole")
  pure (Wormhole w, snd $ unsafePerformIO (readIORef w))

-- | Fill in a wormhole _before_ returning @b@. Normally @b@ will include the other end of the
-- wormhole.
fillWormhole :: HasCallStack => a -> Wormhole a -> b -> b
fillWormhole a w b = unsafePerformIO (fillWormholeIO a w) `seq` b

-- | Fill in a wormhole with the value. A wormhole can only be filled in once. Subsequent attempts
-- will return an error.
fillWormholeIO :: HasCallStack => a -> Wormhole a -> IO ()
fillWormholeIO a (Wormhole w) = do
  filled <- atomicModifyIORef w $ \ h@(filled, _) -> (if filled then h else (True, a), filled)
  if filled
    then error "wormhole already filled"
    else pure ()

main :: IO ()
main = do
  let pv = intPrivy 3
  let pv2 = intPrivy 7
  print (takePrivyValue (pv <> pv2) :: Maybe (Privy, Sum Int))
