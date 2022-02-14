{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase #-}

module Parsing (
	parseFile)
	where

import Language.C (parseCFile,CDecl)
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.SemRep
import Language.C.Analysis.DefTable (DefTable)
import Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_,getDefTable,withDefTable)
import qualified Language.C.Data.Ident as CIdent
import Language.C.Data.Node (lengthOfNode,NodeInfo)
import Language.C.Data.Position (posOf,posFile,posRow,posColumn)
import Text.PrettyPrint.HughesPJ (render)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as ASTMap

import GlobDecls
import AST
import R3Monad

{-
While translating the Language.C AST to our AST, we also do type inference,
so we avoid having to define an intermediate format for an untyped R3 AST, which would
necessarily be different from the typed one (hence could not simply be implemented by a type parameter).
-}

parseFile :: String -> FilePath -> R3 (Either String TranslUnit)
parseFile gcc filepath = do
	parseCFile (newGCC gcc) Nothing [] filepath >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> case runTrav_ $ do
			globaldecls <- analyseAST ctranslunit
			deftable <- getDefTable
			return (globaldecls,deftable)
			of
			Left errs -> return $ Left $ "HARD ERRORS:\n" ++ unlines (map show errs)
			Right ((globaldecls,deftable),_) -> do
				liftIO $ writeFile "GlobDecls.html" $ globdeclsToHTMLString globaldecls
				return $ Right $ globDecls2AST machinespec deftable globaldecls

globDecls2AST :: MachineSpec -> DefTable -> GlobalDecls -> TranslUnit
globDecls2AST machinespec deftable GlobalDecls{..} = ASTMap.map identdecl2extdecl $ ASTMap.mapKeys cident2ident gObjs
	where
{-
	decl2type :: DefTable -> CDecl -> Type
	decl2type deftable decl = case runTrav_ $ do
		withDefTable (const ((),deftable))
		analyseTypeDecl decl
		of
		Left errs    -> error $ show errs
		Right (ty,_) -> ty
-}

	ni2loc :: NodeInfo -> Loc
	ni2loc ni = let pos = posOf ni in Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)

	ident2ast :: CIdent.Ident -> Ident
	ident2ast (CIdent.Ident name i ni) = Ident name i (ni2loc ni)

	ty2ast :: [Attribute] -> Type -> ZType
	ty2ast attrs ty = case

--data VarDeclaration = VarDeclaration Ident ZType Loc deriving (Show)
	vardecl2ast :: VarDecl -> VarDeclaration
	vardecl2ast (VarDecl (VarName ident Nothing) (DeclAttrs _ _ attrs) ty) =
		VarDeclaration (ident2ast ident) (ty2ast attrs ty) (ni2loc ni)

--data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) Stmt) Loc
	identdecl2extdecl :: IdentDecl -> ExtDecl

	-- Function definition
	identdecl2extdecl (FunctionDef (FunDef vardecl stmt ni)) =
		ExtDecl (vardecl2ast vardecl) (Right $ stmt2ast stmt) (ni2loc ni)
--(VarDecl _ (DeclAttrs _ _ attrs) (FunctionType (FunType ret_ty paramdecls False) fun_attrs))

	-- double __builtin_fabs(double);
	identdecl2extdecl (Declaration (Decl (VarDecl _ (DeclAttrs _ _ attrs) ty) ni)) = error "Not yet implemented"

	-- fp_number_type __thenan_df = { CLASS_SNAN, 0, 0, ... }
	identdecl2extdecl (ObjectDef (ObjDef (VarDecl _ (DeclAttrs _ _ attrs) ty) mb_init ni)) = error "Not yet implemented"

	-- enum fp_class_type: CLASS_INFINITY = 4
	identdecl2extdecl (EnumeratorDef (Enumerator ident expr (EnumType sueref enums attrs _) ni)) = error "Not yet implemented"
