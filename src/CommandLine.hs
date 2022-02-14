{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase #-}

module CommandLine (
	runHereIO,
	compileHereIO )
	where

import System.Process
import System.Exit
import System.Directory (withCurrentDirectory)
import System.IO (writeFile)
import System.FilePath (takeDirectory,takeFileName)


compileHereIO :: FilePath -> [String] -> FilePath -> String -> IO (String,String)
compileHereIO compiler args filename src = do
	writeFile filename src
	runHereIO (takeDirectory filename) compiler args

runHereIO :: FilePath -> String -> [String] -> IO (String,String)
runHereIO rundir exefilename args = withCurrentDirectory rundir $ do
	(retcode,sout,serr) <- readProcessWithExitCode (takeFileName exefilename) args ""
	case retcode of
		ExitFailure exitcode -> error $
			"ExitCode " ++ show exitcode ++ " of runHereIO " ++ exefilename ++ "\n" ++
			"STDOUT = " ++ sout ++ "\n" ++
			"STDERR = " ++ serr ++ "\n"
		ExitSuccess -> return (sout,serr)
