{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Exit

import MachineSpec
import Parsing
import R3Monad

main :: IO ()
main = do
	-- When there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	let compiler = "gcc"
	machinespec <- getMachineSpec compiler
	evalStateT mainR3 (R3State compiler machinespec) >>= exitWith

mainR3 :: R3 ExitCode
mainR3 = do
	parseFile "test.c" >>= \case
		Left errmsg -> do
			liftIO $ putStrLn errmsg
			return $ ExitFailure 1
		Right ast -> do
			return ExitSuccess
