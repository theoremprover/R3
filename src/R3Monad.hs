{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase #-}

module R3Monad (
	R3State(..),
	R3,
	getNewNameCnt,
	module Control.Monad.Trans.State.Lazy,
	liftIO,MonadIO )
	where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO,MonadIO)

import MachineSpec


data R3State = R3State {
	compilerR3    :: FilePath,
	machineSpecR3 :: MachineSpec,
	newNameCntR3  :: Int }

getNewNameCnt :: R3 Int
getNewNameCnt = do
	i <- gets newNameCntR3
	modify $ \ s -> s { newNameCntR3 = newNameCntR3 s + 1 }
	return i

type R3 a = StateT R3State IO a
