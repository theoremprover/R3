{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase,UnicodeSyntax,PackageImports #-}

module Parsing (
	parseFile)
	where

import "language-c" Language.C (parseCFile,CDecl)
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.SemRep hiding (Stmt)
import Language.C.Analysis.DefTable (DefTable)
import Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_,getDefTable,withDefTable)
import qualified Language.C.Data.Ident as CIdent
import Language.C.Syntax.AST
import Language.C.Data.Node (lengthOfNode,NodeInfo)
import Language.C.Data.Position (posOf,posFile,posRow,posColumn)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as ASTMap
import Data.List (nub)

import GlobDecls
import AST
import R3Monad
import MachineSpec
import Utils

{-
While translating the Language.C AST to our AST, we also do type inference,
so we avoid having to define an intermediate format for an untyped R3 AST, which would
necessarily be different from the typed one (hence could not simply be implemented by a type parameter).
-}

parseFile :: FilePath -> R3 (Either String TranslUnit)
parseFile filepath = do
	gcc <- gets compilerR3
	liftIO $ parseCFile (newGCC gcc) Nothing [] filepath >>= \case
		Left parseerror   -> return $ Left $ show parseerror
		Right ctranslunit -> case runTrav_ $ do
			globaldecls <- analyseAST ctranslunit
			deftable <- getDefTable
			return (globaldecls,deftable)
			of
			Left errs -> return $ Left $ "HARD ERRORS:\n" ++ unlines (map show errs)
			Right ((globaldecls,deftable),_) -> do
				liftIO $ writeFile "GlobDecls.html" $ globdeclsToHTMLString globaldecls
				machinespec <- getMachineSpec gcc
				let ast = globDecls2AST machinespec deftable globaldecls
				liftIO $ writeFile "AST.html" $ astToHTMLString ast
				return $ Right ast

globDecls2AST :: MachineSpec -> DefTable -> GlobalDecls -> TranslUnit
globDecls2AST MachineSpec{..} deftable GlobalDecls{..} = ASTMap.map identdecl2extdecl $ ASTMap.mapKeys ident2ast gObjs
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

	ty2ast :: Attributes -> Type -> ZType
	ty2ast attrs ty = case ty of
		DirectType tyname _ tyattrs -> case tyname of
			TyIntegral intty -> case (intty,modes) of
				(TyChar,[])     → ZInt 8 True
				(TySChar,[])    → ZInt 8 False
				(TyUChar,[])    → ZInt 8 True
				(TyShort,[])    → ZInt 16 False
				(TyUShort,[])   → ZInt 16 True
				(TyInt,[])      → ZInt intSize False
				(TyInt,["SI"])  → ZInt 32 False
				(TyInt,["DI"])  → ZInt 64 False
				(TyUInt,[])     → ZInt intSize True
				(TyUInt,["SI"]) → ZInt 32 True
				(TyUInt,["DI"]) → ZInt 64 True
				(TyLong,[])     → ZInt longSize False
				(TyULong,[])    → ZInt longSize True
				(TyLLong,[])    → ZInt longLongSize False
				(TyULLong,[])   → ZInt longLongSize True
				other           → error $ "ty2ast " ++ show other ++ " not implemented!"
			TyFloating floatty → case (floatty,modes) of
				(TyFloat,[])       → ZFloat 8 24
				(TyFloat,["SF"])   → ZFloat 8 24
				(TyFloat,["DF"])   → ZFloat 11 53
				(TyDouble,[])      → ZFloat 11 53
				(TyLDouble,[])     → ZFloat 15 113
				other     → error $ "ty2ast " ++ show other ++ " not implemented!"
			where
			modes = nub $ concat $ for (tyattrs++attrs) $ \case
				Attr (CIdent.Ident "mode" _ _) [CVar (CIdent.Ident mode _ _) _] _ -> [mode]
				Attr (CIdent.Ident "fardata" _ _) _ _ -> []
				attr -> error $ "mode: unknown attr " ++ show attr

		other → error $ "ty2ast " ++ show other ++ " not implemented!"


	vardecl2ast :: NodeInfo -> VarDecl -> VarDeclaration
	vardecl2ast ni (VarDecl (VarName ident Nothing) (DeclAttrs _ _ attrs) ty) =
		VarDeclaration (ident2ast ident) (ty2ast attrs ty) (ni2loc ni)

--data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) Stmt) Loc
	identdecl2extdecl :: IdentDecl -> ExtDecl

	-- Function definition
	identdecl2extdecl (FunctionDef (FunDef vardecl stmt ni)) =
		ExtDecl (vardecl2ast ni vardecl) (Right $ stmt2ast stmt) (ni2loc ni)

	-- double __builtin_fabs(double);
	identdecl2extdecl (Declaration (Decl vardecl ni)) =
		ExtDecl (vardecl2ast ni vardecl) (Left Nothing) (ni2loc ni)

	-- fp_number_type __thenan_df = { CLASS_SNAN, 0, 0, ... }
	identdecl2extdecl (ObjectDef (ObjDef vardecl mb_init ni)) =
		ExtDecl (vardecl2ast ni vardecl) (Left $ fmap expr2ast mb_init) (ni2loc ni)

	-- enum fp_class_type: CLASS_INFINITY = 4
	identdecl2extdecl (EnumeratorDef (Enumerator ident expr (EnumType sueref enums attrs _) ni)) = error "Not yet implemented"

	stmt2ast :: CStat -> Stmt
	stmt2ast cstmt = error ""