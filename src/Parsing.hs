module Parsing (
	parseFile)
	where

import Language.C

import AST

parseFile :: String -> FilePath -> IO (Either String AST)
parseFile gcc filepath = do
	readFile filepath >>= parseCFile (newGCC gcc) (Just filepath) >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> return $ Right $ ctranslunit2AST ctranslunit

ctranslunit2AST :: CTranslUnit -> TranslUnit
ctranslunit2AST (CTranslUnit extdecls _) = TranslUnit $ map extdecl2Decl extdecls

extdecl2Decl
