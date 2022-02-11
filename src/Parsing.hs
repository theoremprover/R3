{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase #-}

module Parsing (
	parseFile)
	where

import Language.C (parseCFile,CDecl)
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.SemRep (Type,GlobalDecls)
import Language.C.Analysis.DefTable (DefTable)
import Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_,getDefTable,withDefTable)
import Language.C.Data.Node (lengthOfNode,NodeInfo)
import Language.C.Data.Position (posOf,posFile,posRow,posColumn)
import Text.PrettyPrint.HughesPJ (render)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

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
			Right ((globaldecls,deftable),_) -> do
				writeFile "GlobDecls.html" $ globdeclsToHTMLString globaldecls
				return $ Right $ globDecls2AST deftable globaldecls

globDecls2AST :: DefTable -> GlobalDecls -> AST
globDecls2AST deftable GlobalDecls{..} = Map.map identdecl2extdecl gObjs
	where
	decl2type :: DefTable -> CDecl -> Type
	decl2type deftable decl = case runTrav_ $ do
		withDefTable (const ((),deftable))
		analyseTypeDecl decl
		of
		Left errs    -> error $ show errs
		Right (ty,_) -> ty

	ni2Loc :: NodeInfo -> Loc
	ni2Loc ni = let pos = posOf ni in Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)

	identdecl2extdecl :: IdentDecl -> ExtDecl ()
	identdecl2extdecl (Declaration (Decl (VarDecl _ (DeclAttrs _ _ attrs) ty) ni)) =


{-
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
-}

