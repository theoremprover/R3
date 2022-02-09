module Parsing (
	parseFile)
	where

import Language.C

import AST

parseFile :: String -> FilePath -> IO (Either String AST)
parseFile gcc filepath = do
	readFile filepath >>= parseCFile (newGCC gcc) (Just filepath) >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> case runTrav_ $ do
			globaldecls <- analyseAST ctranslunit
			deftable <- getDefTable
			return (globaldecls,deftable) of
			Left errs → do
				let errs_msg = "ERRORS:\n" ++ unlines (map show errs)
				myErrorIO $ errs_msg
			Right ((globdecls,deftable),soft_errors) → do

ni2Loc :: NodeInfo -> Loc
ni2Loc ni = let Position{..} = posOf ni in Loc posFile posRow posColumn
