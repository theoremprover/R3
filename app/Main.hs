{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase,UnicodeSyntax #-}

module Main where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Exit
import System.FilePath
import System.Environment
import Prettyprinter
import Data.List

import MachineSpec
import Parsing
import R3Monad
import DataTree
import AST
import Transformation

main :: IO ()
main = do
	-- When there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering

	compiler:funname:opts_filenames <- getArgs >>= return . \case
--		[] → "gcc" : "f" : "switchtest.c" : []
		[] → "gcc" : "_fpmul_parts" : "test.c" : []
		args → args

	let
		opts = filter ("-" `isPrefixOf`) opts_filenames
		[filename] = opts_filenames \\ opts

	(mb_extdecl,r3state) <- repl_main compiler funname opts filename
	exitWith $ case mb_extdecl of
		Nothing  → ExitFailure 1
		Just _   → ExitSuccess

repl_main :: String → String → [String] → String → IO (Maybe ExtDecl,R3State)
repl_main compiler funname opts filename = do
	machinespec <- getMachineSpec compiler
	runStateT (mainR3 compiler funname opts filename) (R3State compiler machinespec 1)

mainR3 :: String → String → [String] → String → R3 (Maybe ExtDecl)
mainR3 compiler funname opts filename = do
	parseFile filename >>= \case
		Left errmsg → do
			liftIO $ putStrLn errmsg
			return Nothing
		Right ast → do
			liftIO $ writeFile "translunit.txt" $ prettyTranslUnitString ast
			ast' <- transformAST ast
			liftIO $ writeFile "transformed.txt" $ prettyTranslUnitString ast'

			let fun_body = lookupExtDef funname ast'
			liftIO $ writeFile (funname <.> "html") $ genericToHTMLString fun_body
			return $ Just fun_body

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
