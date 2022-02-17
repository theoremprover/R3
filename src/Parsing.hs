{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase,UnicodeSyntax,PackageImports #-}

module Parsing (
	parseFile)
	where

import "language-c" Language.C (parseCFile,CDecl,pretty)
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.SemRep hiding (Stmt,Expr)
import Language.C.Analysis.DefTable (DefTable)
import Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_,getDefTable,withDefTable)
import qualified Language.C.Data.Ident as CIdent
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (getCInteger)
import Language.C.Data.Node (lengthOfNode,NodeInfo,nodeInfo)
import Language.C.Data.Position (posOf,posFile,posRow,posColumn,isSourcePos)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as ASTMap
import Data.List (nub)
import Text.PrettyPrint (render)

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
		Left parseerror   → return $ Left $ show parseerror
		Right ctranslunit → case runTrav_ $ do
			globaldecls <- analyseAST ctranslunit
			deftable <- getDefTable
			return (globaldecls,deftable)
			of
			Left errs -> return $ Left $ "HARD ERRORS:\n" ++ unlines (map show errs)
			Right ((globaldecls,deftable),_) → do
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
	ni2loc ni = case posOf ni of
		pos | isSourcePos pos → Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
		other                 → NoLoc $ show other

	ident2ast :: CIdent.Ident -> Ident
	ident2ast (CIdent.Ident name i ni) = Ident name i (ni2loc ni)

	ty2ast :: Attributes -> Type -> ZType
	ty2ast attrs ty = case ty of
		DirectType tyname _ tyattrs → case tyname of
			TyIntegral intty → case (intty,modes) of
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
				(TyFloat,[])     → ZFloat 8 24
				(TyFloat,["SF"]) → ZFloat 8 24
				(TyFloat,["DF"]) → ZFloat 11 53
				(TyDouble,[])    → ZFloat 11 53
				(TyLDouble,[])   → ZFloat 15 113
				other            → error $ "ty2ast " ++ show other ++ " not implemented!"
			TyVoid → ZUnit
			TyEnum _ → ZInt intSize False
			TyComp comptyperef → resolve_comptyperef comptyperef where
				resolve_comptyperef (CompTypeRef sueref comptykind _) = case sueref of
					CIdent.AnonymousRef name → error $ "XXX " ++ show comptyperef
					CIdent.NamedRef ident → error $ "XXX " ++ show comptyperef
			_ → ZUnhandled $ (render.pretty) ty
			where
			modes = nub $ concat $ for (tyattrs++attrs) $ \case
				Attr (CIdent.Ident "mode" _ _) [CVar (CIdent.Ident mode _ _) _] _ → [mode]
				Attr (CIdent.Ident "fardata" _ _) _ _                             → []
				attr → error $ "mode: unknown attr " ++ show attr

		ArrayType elem_ty arraysize _ _ → ZArray (ty2ast [] elem_ty) $ case arraysize of
			ArraySize _ (CConst (CIntConst cinteger _)) → Just $ getCInteger cinteger
			UnknownArraySize _                          → Nothing

		PtrType target_ty _ _ → ZPtr $ ty2ast [] target_ty

		FunctionType (FunType target_ty paramdecls is_variadic) _ →
			ZFun (ty2ast [] target_ty) is_variadic $ map ((ty2ast []).declType) paramdecls

		TypeDefType (TypeDefRef _ innerty _) _ attribs → ty2ast (attribs++attrs) innerty

		other → error $ "ty2ast " ++ show other ++ " not implemented!"


	decl2ast :: (Declaration d) => NodeInfo -> d -> VarDeclaration
	decl2ast ni decl = VarDeclaration (ident2ast ident) ((render.pretty) ty) (ty2ast attrs ty) (ni2loc ni)
		where
		VarDecl (VarName ident Nothing) (DeclAttrs _ _ attrs) ty = getVarDecl decl

--data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) Stmt) Loc
	identdecl2extdecl :: IdentDecl -> ExtDecl

	-- Function definition
	identdecl2extdecl identdecl = ExtDecl (decl2ast ni identdecl) body (ni2loc ni)
		where
		ni = nodeInfo identdecl
		body = case identdecl of
			FunctionDef (FunDef vardecl stmt _)   → Right $ stmt2ast stmt
			Declaration (Decl vardecl _)          → Left Nothing
			ObjectDef (ObjDef vardecl mb_init _)  → Left $ case mb_init of
				Nothing                     → Nothing
				Just (CInitExpr expr _)     → Just $ expr2ast expr
				Just (CInitList initlist _) → error "CInitList not implemented yet"
			EnumeratorDef (Enumerator _ expr _ _)  → Left $ Just $ expr2ast expr

	stmt2ast :: CStat -> Stmt
	stmt2ast cstmt = ExprStmt (Var (Ident "DUMMY" 0 (ni2loc $ nodeInfo cstmt)) ZUnit (ni2loc $ nodeInfo cstmt)) (ni2loc $ nodeInfo cstmt)  --DUMMY

	expr2ast :: CExpr -> Expr
	expr2ast expr = Var (Ident "DUMMY" 0 (ni2loc $ nodeInfo expr)) ZUnit (ni2loc $ nodeInfo expr)