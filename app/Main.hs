{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Exit

import Parsing

main :: IO ()
main = do
	-- When there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	exitval <- parseFile "gcc" "test.c" >>= \case
		Left errmsg -> do
			putStrLn errmsg
			return $ ExitFailure 1
		Right ast -> do
			return ExitSuccess

	exitWith exitval
