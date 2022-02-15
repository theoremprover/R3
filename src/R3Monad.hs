{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase #-}

module R3Monad (
	R3State(..),
	R3,
	module Control.Monad.Trans.State.Lazy,
	liftIO,MonadIO )
	where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO,MonadIO)

import MachineSpec


data R3State = R3State {
	compilerR3 :: FilePath,
	machineSpecR3 :: MachineSpec }

type R3 a = StateT R3State IO a
