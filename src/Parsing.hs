module Parsing (
	parseFile_CLanguage)
	where

import Language.C

import AST

parseFile_CLanguage :: String -> FilePath -> IO (Either String AST)
parseFile_CLanguage gcc filepath = do
	readFile filepath >>= parseCFile (newGCC gcc) (Just filepath) >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> return $ languagecToAST ctranslunit

languagecToAST :: CTranslUnit -> TranslationUnit
languagecToAST ctranslunit = return $ TranslationUnit
