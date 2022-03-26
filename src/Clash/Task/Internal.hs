-----------------------------------------------------------------------------
-- |
-- Module      :  Clash.Task.Internal
-- Copyright   :  (C) 2021 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Unsafe and unstable internals to the task structure.
--
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Clash.Task.Internal where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Catch       (MonadCatch (..), MonadThrow (..))
import           Control.Monad.Except      (MonadError (..))
import qualified Control.Monad.Fail        as F (MonadFail (fail))
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Identity
import           Control.Monad.Reader      (MonadReader (..))
import           Control.Monad.State
import           Control.Monad.State       (MonadState (..))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Writer      (MonadWriter (..), censor)
import qualified Data.Foldable             as F
import           Data.Functor
import           Data.IORef
import           Data.List                 hiding (foldr)
import qualified Data.List                 as L
import           Data.Maybe
import           System.IO.Unsafe

-- | A task is a way of declaring how to interact with a signal. Tasks are
-- suitable to use with clash 'Signal's because they are lazy enough to handle
-- same cycle communication.
--
-- Tasks are normally declared in do notation and if you squint they look a
-- little like tasks in Verilog.
--
-- @
-- driveReadRequest :: Task (Bool, Word8) Bool ()
-- driveReadRequest = do
--   wait 5 (False, 0xff)
--   forM_ [0xaa,0xbb,0xcc] $ \x -> do
--     await (True, 0xaabb) id
--     n <- genRandom 0 5
--     wait n (False, error "invalid")
--   awaitM \_ -> gets (>= 3)
--   await (True, 0xaabb) id
--   forever (False, 0xff)
--
-- driveReadResponse :: Task Bool (Bool, Word32) ()
-- driveReadResponse = do
--   awaitMaybe False (bool Nothing (Just True) . fst) -- ready = valid
--   sets (+1)
-- @
--
-- Separate Tasks can be combined so you can declare a Task for a single
-- channel and combine them later, synchronising between the channels using a
-- monad (State for example).
data Task fw bw m a
  = Pure a
  | Take (bw -> Taken fw bw m a)
  | M (m (Task fw bw m a))

data Taken fw bw m a
  = TM (m (Taken fw bw m a))
  | Give fw (Task fw bw m a)

instance Functor m => Functor (Task bw fw m) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Take s) = Take $ \bw -> fmap f (s bw)
  fmap f (M m)    = M $ fmap f <$> m

instance Functor m => Functor (Taken bw fw m) where
  fmap f (TM m)      = TM $ fmap f <$> m
  fmap f (Give fw p) = Give fw (fmap f p)

instance Functor m => Applicative (Task b f m) where
  pure = Pure
  pf <*> pa = go pf where
    go p = case p of
      Take s -> Take (\bw -> goTaken (s bw))
      M m    -> M (fmap go m)
      Pure f -> fmap f pa
    goTaken = \case
      TM m      -> TM (fmap goTaken m)
      Give fw p -> Give fw (go p)

instance Functor m => Monad (Task b f m) where
  p0 >>= f = go p0 where
    go = \case
      Pure r -> f r
      Take s -> Take (\bw -> goTaken (s bw))
      M m    -> M $ go <$> m
    goTaken = \case
      TM m      -> TM (fmap goTaken m)
      Give fw p -> Give fw (go p)

instance MonadTrans (Task fw bw) where
  lift = M . fmap Pure

instance F.MonadFail m => F.MonadFail (Task bw fw m) where
  fail = lift . F.fail

instance MonadIO m => MonadIO (Task bw fw m) where
  liftIO = M . liftIO . fmap Pure

instance MonadReader r m => MonadReader r (Task bw fw m) where
  ask = lift ask
  local f = go where
    go = \case
      Pure r -> Pure r
      Take s -> Take (\bw -> goTaken (s bw))
      M m    -> M (go <$> local f m)
    goTaken p = case p of
      Give fw p -> Give fw (go p)
      TM m      -> TM (goTaken <$> local f m)
  reader = lift . reader

instance MonadState s m => MonadState s (Task bw fw m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter w m => MonadWriter w (Task bw fw m) where
  writer = lift . writer
  tell = lift . tell
  listen p0 = go p0 mempty where
    go p w = case p of
      Take s -> Take (\bw -> goTaken (s bw) w)
      M m -> M (do
        (p', w') <- listen m
        return (go p' $! mappend w w') )
      Pure r -> Pure (r, w)
    goTaken p w = case p of
      Give fw p -> Give fw (go p w)
      TM m -> TM (do
        (p', w') <- listen m
        return (goTaken p' $! mappend w w') )

  pass p0 = go p0 mempty where
    go p w = case p of
        Take s -> Take (\bw -> goTaken (s bw) w)
        M m -> M $ do
          (p', w') <- censor (const mempty) (listen m)
          return (go p' $! mappend w w')
        Pure   (r, f)  -> M (pass (return (Pure r, \_ -> f w)))
    goTaken p w = case p of
      Give fw t -> Give fw (go t w)
      TM m -> TM $ do
        (p', w') <- censor (const mempty) (listen m)
        return (goTaken p' $! mappend w w')

instance MonadError e m => MonadError e (Task bw fw m) where
  throwError = lift . throwError
  catchError p0 f = go p0 where
    go p = case p of
      Take s -> Take (\bw -> goTaken bw (s bw))
      Pure r -> Pure r
      M m    -> M $ fmap go m `catchError` (pure . f)
    goTaken bw = \case
      Give fw p -> Give fw (go p)
      -- a best effort case, might be better just to error outright
      TM m -> TM $ (fmap (goTaken bw) m `catchError` (pure . tryRecover . f))
      where tryRecover = \case
              Take s -> (s bw)
              Pure r -> (Give (error "thrown while taken") (Pure r))
              M m    -> (TM $ tryRecover <$> m)

instance MonadThrow m => MonadThrow (Task bw fw m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (Task bw fw m) where
  catch p0 f = go p0 where
    go p = case p of
      Take s -> Take (\bw -> goTaken bw (s bw))
      Pure r -> Pure r
      M m    -> M $ fmap go m `Control.Monad.Catch.catch` (pure . f)
    goTaken bw = \case
      Give fw p -> Give fw (go p)
      -- a best effort case, might be better just to error outright
      TM m -> TM $ (fmap (goTaken bw) m `Control.Monad.Catch.catch` (pure . tryRecover . f))
      where tryRecover = \case
              Take s -> (s bw)
              Pure r -> (Give (error "thrown while taken") (Pure r))
              M m    -> (TM $ tryRecover <$> m)

fwMap :: Functor m => (fw -> fw') -> Task fw bw m a -> Task fw' bw m a
fwMap f p0 = go p0 where
  go = \case
    Pure a -> Pure a
    Take s -> Take (\bw -> goTaken (s bw))
    M m    -> M $ fmap go m
  goTaken = \case
    TM m      -> TM $ fmap goTaken m
    Give fw p -> Give (f fw) (go p)

bwMap :: Functor m => (bw' -> bw) -> Task fw bw m a -> Task fw bw' m a
bwMap f p0 = go p0 where
  go = \case
    Pure a -> Pure a
    Take s -> Take (\bw -> goTaken (s (f bw)))
    M m    -> M $ fmap go m
  goTaken = \case
    Give fw p -> Give fw (go p)
    TM m      -> TM $ fmap goTaken m

-- | Combine two tasks. This can be used to combine tasks that work on separate channels (e.g. a
-- request and a response).
--
-- Be aware that monadic actions will be clumsily interleaved in a given cycle, starting with the first
-- task's action. Currently there is no good way to synchronise between tasks within the same cycle,
-- however between cycles the monads will be synchonised.
--
-- The resulting task will finish when either of the tasks finish.
combineWith
  :: Applicative m
  => (fw1 -> fw2 -> fw')
  -> (bw' -> (bw1, bw2))
  -> Task fw1 bw1 m a
  -> Task fw2 bw2 m b
  -> Task fw' bw' m ()
combineWith fwF bwF = go where
  go p1 p2 = case (p1, p2) of
    (Take s1, Take s2) -> Take $ \ bw -> let (bw1, bw2) = bwF bw in goTaken (s1 bw1) (s2 bw2)
    (M m1, M m2) -> M $ liftA2 (\p1' p2' -> go p1' p2') m1 m2
    (M m1, p2') -> M $ m1 <&> \p1' -> go p1' p2'
    (p1', M m2) -> M $ m2 <&> \p2' -> go p1' p2'
    _ -> Pure ()
  goTaken p1 p2 = case (p1, p2) of
    (Give fw1 t1, Give fw2 t2) -> Give (fwF fw1 fw2) (go t1 t2)
    (TM m1, TM m2) -> TM $ liftA2 (\p1' p2' -> goTaken p1' p2') m1 m2
    (TM m1, p2') -> TM $ m1 <&> \p1' -> goTaken p1' p2'
    (p1', TM m2) -> TM $ m2 <&> \p2' -> goTaken p1' p2'

-- combineWithIso
--   :: AnIso sBw sFw (aBw, bBw) (aFw, bFw)
--   -> Task aFw aBw ()
--   -> Task bFw bBw ()
--   -> Task sFw sBw ()
-- combineWithIso i = withIso i (\v c -> combineWith (\a b -> c (a,b)) v)

data Jet a = a :- Jet a

run :: Jet bw -> Task fw bw Identity a -> [fw]
run j = runIdentity . runM j

runM :: Monad m => Jet bw -> Task fw bw m a -> m [fw]
runM bws0 p0 = goTake bws0 p0 where
  goTake bss@ ~(bw :- bws) p = case p of
    Pure _ -> pure []
    Take s -> goGive (s bw) >>= \case
                ~(fw, p) -> (fw :) <$> goTake bws p
    M m -> m >>= goTake bss
  goGive p = case p of
    Give fw p -> pure (fw, p)
    TM m      -> m >>= goGive

runUncons :: Monad m => (bws -> (bw, bws)) -> bws -> Task fw bw m a -> m [fw]
runUncons uncons = goTake where
  goTake bws = \case
    Pure _ -> pure []
    Take s -> let (bw, bws') = uncons bws
              in  goGive (s bw) >>= \case
                    ~(fw, t) -> (fw :) <$> goTake bws' t
    M m -> m >>= goTake bws
  goGive = \case
    Give fw t -> pure (fw, t)
    TM m      -> m >>= goGive

runLockstep :: forall bws bw fw a. (bws -> (fw -> bw, bws)) -> bws -> Task fw bw Identity a -> [(bw, fw)]
runLockstep uncons = goTake where
  goTake :: bws -> Task fw bw Identity a -> [(bw, fw)]
  goTake bws = \case
    Pure _ -> []
    Take s -> let (bwF, bws') = uncons bws
                  bw = bwF fw
                  ~(fw, t) = goGive (s bw)
              in  (bw, fw) : goTake bws' t
    -- M (Identity t) -> goTake bws t
  goGive = \case
    Give fw t       -> (fw, t)
    TM (Identity t) -> goGive t

class Monad m => Interleave m where
  unsafeInterleaveM :: m a -> m a

instance Interleave IO where
  unsafeInterleaveM = unsafeInterleaveIO

instance Interleave m => Interleave (StateT s m) where
  unsafeInterleaveM = mapStateT unsafeInterleaveM

l2j ~(a:as) = a :- l2j as

runM' :: MonadIO m => (Jet fw -> Jet bw) -> Task fw bw m a -> m ()
runM' f t = do
  fwsRef <- liftIO $ newIORef (error "fws not set")
  let bws = f (l2j $ unsafePerformIO (readIORef fwsRef))
  fws <- runM bws t
  liftIO $ writeIORef fwsRef fws

execInterleave :: (Interleave m, MonadIO m) => Jet bw -> Task fw bw m a -> m [fw]
execInterleave bws0 p0 = goTake bws0 p0 where
  goTake bss@ ~(bw :- bws) p = case p of
    Pure _ -> pure []
    Take s -> goGive (s bw) >>= \case
                ~(fw, p) -> (fw :) <$> unsafeInterleaveM (goTake bws p)
    M m -> m >>= goTake bss
  goGive p = case p of
    Give fw p -> pure (fw, p)
    TM m      -> m >>= unsafeInterleaveM . goGive

runInterleave :: (Interleave m, MonadIO m) => (Jet fw -> Jet bw) -> Task fw bw m a -> m ()
runInterleave f t = do
  bwsRef <- liftIO $ newIORef (error "bws not set")
  fws <- execInterleave (unsafePerformIO (readIORef bwsRef)) t
  let bws = f (l2j fws)
  liftIO $ writeIORef bwsRef bws
  length fws `seq` pure ()

next :: fw -> Task fw bw m bw
next fw = Take $ \bw -> Give fw (Pure bw)

-- | Give a fw depending on the bw at that cycle.
nextWithBw :: (bw -> fw) -> Task fw bw m (bw, fw)
nextWithBw f = Take $ \bw -> let fw = f bw in Give fw (Pure (bw, fw))

-- | Give a fw depending on the bw at that cycle with a monadic action. The monadic action should
-- not throw because the fw value is owed.
nextWithBwM :: Functor m => (bw -> m fw) -> Task fw bw m (bw, fw)
nextWithBwM f = Take $ \bw -> TM $ f bw <&> \fw -> Give fw (Pure (bw, fw))

-- | Keep giving the same forward value and then check if a matching backward is seen. This means
-- you'll be in the _cycle after_ the matching backward when this returns. If you need to give a fw
-- depending on the bw use `nextWithBw` or `awaitMaybe`.
await :: fw -> (bw -> Bool) -> Task fw bw m (bw, fw)
await fw p = Take $ \bw -> Give fw (if p bw then Pure (bw, fw) else await fw p)

-- | 'await' with a monadic action.
awaitM :: Functor m => fw -> (bw -> m Bool) -> Task fw bw m ()
awaitM fw p = Take $ \bw -> Give fw . M $ p bw <&> \b -> if b then Pure () else awaitM fw p

-- | Wait for @n@ cycles giving the same `fw` each time.
wait :: fw -> Int -> Task fw bw m ()
wait _ 0  = Pure ()
wait fw n = Take $ \_bw -> Give fw (wait fw (n - 1))

-- | Wait for @n@ cycles giving the same `fw` each time.
waitM :: Functor m => m fw -> Int -> Task fw bw m ()
waitM _ 0   = Pure ()
waitM fwM n = M $ fwM <&> \fw -> Take $ \_bw -> Give fw (waitM fwM (n - 1))

forever :: fw -> Task fw bw m a
forever fw = go where
  go = Take $ \_bw -> Give fw go

-- | Await on a backward value and produce a fw for that cycle, given a default value to use while
-- returning Nothing.
awaitMaybe :: fw -> (bw -> Maybe fw) -> Task fw bw m ()
awaitMaybe fwDefault f = go where
  go = Take $ \bw ->
    case f bw of
      Just fw -> Give fw (Pure ())
      Nothing -> Give fwDefault go

-- | Await on a backward value and produce a fw for that cycle. If you throw an MonadError during
-- the @m fw@, then the forward value will be an error.
awaitMaybeM :: Functor m => fw -> (bw -> Maybe (m fw)) -> Task fw bw m ()
awaitMaybeM fwDefault f = go where
  go = Take $ \bw ->
    case f bw of
      Just mfw -> TM $ mfw <&> \fw -> Give fw (Pure ())
      Nothing  -> Give fwDefault go

-- | Await on a backward value and produce a fw for that cycle. If you throw an MonadError during
-- the @m fw@, then the forward value will be an error.
awaitMMaybe :: Functor m => fw -> (bw -> m (Maybe fw)) -> Task fw bw m ()
awaitMMaybe fwDefault f = go where
  go = Take $ \bw -> TM $ f bw <&> \case
    Just fw -> Give fw (Pure ())
    Nothing -> Give fwDefault go
