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

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Clash.Task.Internal where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
import qualified Data.Foldable       as F
import           Data.Functor
import           Data.List           hiding (foldr)
import qualified Data.List           as L
import           Data.Maybe
import           Debug.Trace

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
  | Take (bw -> Task fw bw m a)
  | Give fw (Task fw bw m a)
  | M (m (Task fw bw m a))

instance Functor m => Functor (Task b f m) where
  fmap f (Pure a)    = Pure (f a)
  fmap f (Take s)    = Take $ \bw -> fmap f (s bw)
  fmap f (Give fw p) = Give fw (fmap f p)
  fmap f (M m)       = M $ fmap f <$> m

instance Functor m => Applicative (Task b f m) where
  pure = Pure
  pf <*> pa = go pf where
    go p = case p of
      Take s    -> Take (\bw -> go (s bw))
      Give fw p -> Give fw (go p)
      M m       -> M (fmap go m)
      Pure f    -> fmap f pa

instance Functor m => Monad (Task b f m) where
  p0 >>= f = go p0 where
    go p = case p of
      Pure r    -> f r
      Give fw p -> Give fw (go p)
      Take s    -> Take (\bw -> go (s bw))
      M m       -> M $ go <$> m

instance MonadTrans (Task fw bw) where
  lift = M . fmap Pure


-- instance F.MonadFail m => F.MonadFail (Task bw fw m) where
--     fail = lift . F.fail

-- instance MonadIO m => MonadIO (Task bw fw m) where
--     liftIO m = M (liftIO (Pure <$> m))

-- instance MonadReader r m => MonadReader r (Task bw fw m) where
--     ask = lift ask
--     local f = go
--         where
--           go p = case p of
--               Request a' fa  -> Request a' (\a  -> go (fa  a ))
--               Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
--               Pure    r      -> Pure r
--               M       m      -> M (go <$> local f m)
--     reader = lift . reader

-- instance MonadState s m => MonadState s (Task bw fw m) where
--     get = lift get
--     put = lift . put
--     state = lift . state

-- instance MonadWriter w m => MonadWriter w (Task bw fw m) where
--     writer = lift . writer
--     tell = lift . tell
--     listen p0 = go p0 mempty
--       where
--         go p w = case p of
--             Request a' fa  -> Request a' (\a  -> go (fa  a ) w)
--             Respond b  fb' -> Respond b  (\b' -> go (fb' b') w)
--             M       m      -> M (do
--                 (p', w') <- listen m
--                 return (go p' $! mappend w w') )
--             Pure    r      -> Pure (r, w)

--     pass p0 = go p0 mempty
--       where
--         go p w = case p of
--             Request a' fa  -> Request a' (\a  -> go (fa  a ) w)
--             Respond b  fb' -> Respond b  (\b' -> go (fb' b') w)
--             M       m      -> M (do
--                 (p', w') <- censor (const mempty) (listen m)
--                 return (go p' $! mappend w w') )
--             Pure   (r, f)  -> M (pass (return (Pure r, \_ -> f w)))

-- instance MonadError e m => MonadError e (Task bw fw m) where
--     throwError = lift . throwError
--     catchError p0 f = go p0
--       where
--         go p = case p of
--             Request a' fa  -> Request a' (\a  -> go (fa  a ))
--             Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
--             Pure    r      -> Pure r
--             M          m   -> M ((do
--                 p' <- m
--                 return (go p') ) `catchError` (\e -> return (f e)) )

-- instance MonadThrow m => MonadThrow (Task bw fw m) where
--     throwM = lift . throwM
--     {-# INLINE throwM #-}

-- instance MonadCatch m => MonadCatch (Task bw fw m) where
--     catch p0 f = go p0
--       where
--         go p = case p of
--             Request a' fa  -> Request a' (\a  -> go (fa  a ))
--             Respond b  fb' -> Respond b  (\b' -> go (fb' b'))
--             Pure    r      -> Pure r
--             M          m   -> M ((do
--                 p' <- m
--                 return (go p') ) `Control.Monad.Catch.catch` (\e -> return (f e)) )


fwMap :: Functor m => (fw -> fw') -> Task fw bw m a -> Task fw' bw m a
fwMap f p0 = go p0 where
  go p = case p of
    Pure a    -> Pure a
    Give fw p -> Give (f fw) (go p)
    Take s    -> Take (\bw -> go (s bw))
    M m       -> M $ fmap go m

bwMap :: Functor m => (bw' -> bw) -> Task fw bw m a -> Task fw bw' m a
bwMap f p0 = go p0 where
  go p = case p of
    Pure a    -> Pure a
    -- Step fw s -> Step fw (\bw -> go (s (f bw)))
    Give fw p -> Give fw (go p)
    Take s    -> Take (\bw -> go (s (f bw)))
    M m       -> M $ fmap go m

combineWith
  :: Applicative m
  => (fw1 -> fw2 -> fw')
  -> (bw' -> (bw1, bw2))
  -> Task fw1 bw1 m a
  -> Task fw2 bw2 m b
  -> Task fw' bw' m ()
combineWith fwF bwF p1 p2 =
  case (p1, p2) of
    (Take s1, Take s2) -> Take $ \ bw -> let (bw1, bw2) = bwF bw in combineWith fwF bwF (s1 bw1) (s2 bw2)
    (Give fw1 p1, Give fw2 p2) -> Give (fwF fw1 fw2) $ combineWith fwF bwF p1 p2
    (M m1, M m2) -> M $ liftA2 (\p1' p2' -> combineWith fwF bwF p1' p2') m1 m2
    (M m1, p2') -> M $ m1 <&> \p1' -> combineWith fwF bwF p1' p2'
    (p1', M m2) -> M $ m2 <&> \p2' -> combineWith fwF bwF p1' p2'
    -- (Step fw1 s1, Step fw2 s2) ->
    --   Step (fwF fw1 fw2) $ \bw -> let (bw1, bw2) = bwF bw in combineWith fwF bwF (s1 bw1) (s2 bw2)
    _ -> Pure ()

data Jet a = a :- Jet a

runM :: Monad m => Jet bw -> Task fw bw m a -> m [fw]
runM bws0 p0 = goTake bws0 p0 where
  goTake bss@(bw :- bws) p = case p of
    Pure _ -> pure []
    Take s -> goGive (s bw) >>= \case
                ~(fw, p) -> (fw :) <$> goTake bws p
    M m -> m >>= goTake bss
  goGive p = case p of
    Give fw p -> pure (fw, p)
    M m       -> m >>= goGive
    Take _    -> error "You took twice in a row!"
    Pure _    -> error "Did a pure after a take!"

next :: fw -> Task fw bw m bw
next fw = Take $ \bw -> Give fw (Pure bw)

await :: fw -> (bw -> Bool) -> Task fw bw m ()
await fw p = Take $ \bw -> Give fw (if p bw then Pure () else await fw p)

awaitM :: Functor m => fw -> (bw -> m Bool) -> Task fw bw m ()
awaitM fw p = Take $ \bw -> M $ p bw <&> \b -> if b then Pure () else awaitM fw p

wait :: fw -> Int -> Task fw bw m ()
wait _ 0  = Pure ()
wait fw n = Take $ \_bw -> Give fw (wait fw (n - 1))

forever :: fw -> Task fw bw m a
forever fw = go where
  go = Take $ \_bw -> Give fw go

awaitMaybe :: fw -> (bw -> Maybe fw) -> Task fw bw m ()
awaitMaybe fwDefault f = go where
  go = Take $ \bw ->
    case f bw of
      Just fw -> Give fw (Pure ())
      Nothing -> Give fwDefault go

awaitMaybeM :: Functor m => fw -> (bw -> Maybe (m fw)) -> Task fw bw m ()
awaitMaybeM fwDefault f = go where
  go = Take $ \bw ->
    case f bw of
      Just mfw -> M $ mfw <&> \fw -> Give fw (Pure ())
      Nothing  -> Give fwDefault go

awaitMMaybe :: Functor m => fw -> (bw -> m (Maybe fw)) -> Task fw bw m ()
awaitMMaybe fwDefault f = go where
  go = Take $ \bw -> M $ f bw <&> \case
    Just fw -> Give fw (Pure ())
    Nothing -> Give fwDefault go

-- combineWithIso
--   :: AnIso sBw sFw (aBw, bBw) (aFw, bFw)
--   -> Task aFw aBw ()
--   -> Task bFw bBw ()
--   -> Task sFw sBw ()
-- combineWithIso i read write = (\_ -> ()) <$> withIso i (\v c -> combineWith (\a b -> c (a,b)) v read write)
