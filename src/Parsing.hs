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

import GlobDecls
import AST

parseFile :: String -> FilePath -> IO (Either String AST)
parseFile gcc filepath = do
	parseCFile (newGCC gcc) Nothing [] filepath >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> case runTrav_ $ do
			globaldecls <- analyseAST ctranslunit
			deftable <- getDefTable
			return (globaldecls,deftable)
			of
			Left errs -> return $ Left $ "ERRORS:\n" ++ unlines (map show errs)
			Right ((globdecls,deftable),_) -> do
				writeFile "GlobDecls.html" $ globdeclsToHTMLString globdecls
				return $ Right $ 

decl2TypeM :: CDecl → CovVecM Type
decl2TypeM deftable decl = do
	case runTrav_ (withDefTable (const ((),deftable)) >> myAnalyseTypeDecl decl) of
		Right (ty,_) → return ty
		Left errs → myError $ show errs
	where
	-- taken from Language/C/Analysis/DeclAnalysis.hs, added proper handling of initializers
	myAnalyseTypeDecl :: (MonadTrav m) => CDecl → m Type
	myAnalyseTypeDecl (CDecl declspecs declrs node) = case declrs of
		[] → analyseTyDeclr (CDeclr Nothing [] Nothing [] node)
		(Just declr,_,Nothing):_ → analyseTyDeclr declr
		where
		analyseTyDeclr (CDeclr _ derived_declrs Nothing attrs _declrnode) = do
			canonTySpecs <- canonicalTypeSpec typespecs
			t <- tType True node (map CAttrQual (attrs++attrs_decl) ++ typequals) canonTySpecs derived_declrs []
			case nameOfNode node of
				Just n → withDefTable (\dt → (t, insertType dt n t))
				Nothing → return t
			where
			(storagespec, attrs_decl, typequals, typespecs, funspecs, alignspecs) = partitionDeclSpecs declspecs
		analyseTyDeclr other = error $ "analyseTyDeclr " ++ show other

ni2Loc :: NodeInfo -> Loc
ni2Loc ni = let pos = posOf ni in Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
