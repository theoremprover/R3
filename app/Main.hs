{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE LambdaCase,UnicodeSyntax #-}

module Main where

import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import System.Exit
import System.FilePath
import Prettyprinter

import MachineSpec
import Parsing
import R3Monad
import DataTree
import AST
import Transformation
import TypeInference

function_name = "_fpmul_parts"
file_name = "test.c"

main :: IO ()
main = do
	-- When there is an error, we'd like to have *all* output till then
	hSetBuffering stdout NoBuffering
	main1 >>= \case
		Nothing  → exitWith $ ExitFailure 1
		Just ast → exitWith ExitSuccess

main1 :: IO (Maybe (ExtDecl ZType))
main1 = do
	let compiler = "gcc"
	machinespec <- getMachineSpec compiler
	evalStateT mainR3 (R3State compiler machinespec 1)

mainR3 :: R3 (Maybe (ExtDecl ZType))
mainR3 = do
--	liftIO $ writeFile ("test.html") $ genericToHTMLString testG
	parseFile file_name >>= \case
		Left errmsg → do
			liftIO $ putStrLn errmsg
			return Nothing
		Right raw_ast → do
			typed_ast <- inferTypes raw_ast
			ast <- transformAST typed_ast
			let fun_body = lookupExtDef function_name ast'
			liftIO $ writeFile (function_name <.> "html") $ genericToHTMLString fun_body
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
