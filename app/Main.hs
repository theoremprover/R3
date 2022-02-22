{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Exit
import System.FilePath

import MachineSpec
import Parsing
import R3Monad
import DataTree
import AST
import qualified Data.Map.Strict as ASTMap

function_name = "_fpmul_parts"
file_name = "test.c"

main :: IO ()
main = do
	-- When there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	let compiler = "gcc"
	machinespec <- getMachineSpec compiler
	evalStateT mainR3 (R3State compiler machinespec) >>= exitWith

mainR3 :: R3 ExitCode
mainR3 = do
	liftIO $ writeFile ("test.html") $ genericToHTMLString testG
	parseFile file_name >>= \case
		Left errmsg -> do
			liftIO $ putStrLn errmsg
			return $ ExitFailure 1
		Right ast -> do
			let Just fun_body = ASTMap.lookup function_name $ ASTMap.mapKeys nameIdent ast
			liftIO $ writeFile (function_name <.> "html") $ genericToHTMLString fun_body
			return ExitSuccess

{-
type S1 = M1 S
type C1 = M1 C
type D1 = M1 D

*DataTree> :t (from [[1,2],[3,4]])
(from [[1,2],[3,4]])
  :: Num a =>
     D1
       ('MetaData "[]" "GHC.Types" "ghc-prim" 'False)
       (C1 ('MetaCons "[]" 'PrefixI 'False) U1
        :+: C1
              ('MetaCons ":" ('InfixI 'LeftAssociative 9) 'False)
              (S1
                 ('MetaSel
                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                 (Rec0 [a])
               :*: S1
                     ('MetaSel
                        'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                     (Rec0 [[a]])))
       x

*DataTree> from [[1,2],[3,4]]
M1 {unM1 =
	R1 (
		M1 {unM1 =
			M1 {unM1 =
				K1 {unK1 =
					[1,2] }}
			:*:
			M1 {unM1 =
				K1 {unK1 =
					[[3,4]] }}
			}
		)
	}
-}
