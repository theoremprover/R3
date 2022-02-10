{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase #-}

module Parsing (
	parseFile)
	where

import Language.C
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_)
import Language.C.Data.Node (lengthOfNode)
import Text.PrettyPrint.HughesPJ (render)
import Data.Maybe (fromJust)

import AST

parseFile :: String -> FilePath -> IO (Either String ())
parseFile gcc filepath = do
	parseCFile (newGCC gcc) Nothing [] filepath >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> case runTrav_ $ do
			globaldecls <- analyseAST ctranslunit
--			deftable <- getDefTable
			return globaldecls
			of
			Left errs -> return $ Left $ "ERRORS:\n" ++ unlines (map show errs)
			Right (globdecls,soft_errors) -> do
				putStrLn $ (render.pretty) globdecls
				return $ Right ()

ni2Loc :: NodeInfo -> Loc
ni2Loc ni = let pos = posOf ni in Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
