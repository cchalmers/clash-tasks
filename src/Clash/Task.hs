-----------------------------------------------------------------------------
-- |
-- Module      :  Clash.Task
-- Copyright   :  (C) 2021 Christopher Chalmers
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Christopher Chalmers
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Unsynthesisable tasks for driving clash simulations
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

module Clash.Task (
  Task,
  fwMap,
  bwMap,
  run,
  runM,
  runUncons,
  runLockstep,
  next,
  nextWithBw,
  nextWithBwM,
  onEveryCycle,
  await,
  awaitM,
  wait,
  waitM,
  forever,
  awaitMaybe,
  awaitMaybeM,
  awaitMMaybe,
  ) where

import Clash.Task.Internal
