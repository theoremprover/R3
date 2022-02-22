{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase,UnicodeSyntax,PackageImports #-}

module Parsing (
	parseFile)
	where

import Language.C (parseCFile,CDecl,pretty)
--import "language-c" Language.C (parseCFile,CDecl,pretty)
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
import Data.List (nub)
import Text.PrettyPrint (render)
import qualified Data.Map.Strict as ASTMap

import GlobDecls
import AST
import qualified Data.Map.Strict as ASTMap
import R3Monad
import MachineSpec
import Utils
import DataTree

{-
While translating the Language.C AST to our AST, we also do type inference,
so we avoid having to define an intermediate format for an untyped R3 AST, which would
necessarily be different from the typed one (hence could not simply be implemented by a type parameter).
-}

parseFile :: FilePath → R3 (Either String TranslUnit)
parseFile filepath = do
	gcc <- gets compilerR3
	liftIO $ parseCFile (newGCC gcc) Nothing [] filepath >>= \case
		Left parseerror   → return $ Left $ show parseerror
		Right ctranslunit → do
			writeFile "LanguageC_AST.html" $ genericToHTMLString ctranslunit
			case runTrav_ $ do
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

globDecls2AST :: MachineSpec → DefTable → GlobalDecls → TranslUnit
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

	ni2loc :: NodeInfo → Loc
	ni2loc ni = case posOf ni of
		pos | isSourcePos pos → Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
		other                 → NoLoc $ show other

	ident2ast :: CIdent.Ident → Ident
	ident2ast (CIdent.Ident name i ni) = Ident name i (ni2loc ni)

	ty2ast :: Attributes → Type → ZType
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
			TyVoid             → ZUnit
			TyEnum enumtyperef → resolve_sueref enumtyperef
			TyComp comptyperef → resolve_sueref comptyperef
			_                  → ZUnhandled $ (render.pretty) ty

			where

			resolve_sueref :: (HasSUERef a) => a → ZType
			resolve_sueref hassueref = case ASTMap.lookup sueref gTags of
				Nothing → error $ "Could not find " ++ show sueref ++ " in gTags"
				Just (CompDef (CompType _ comptykind memberdecls attrs ni)) →
					ZCompound (compkind2comptype comptykind) (map (decl2ast ni) memberdecls)
				Just (EnumDef (EnumType _ enumerators attrs ni)) →
					ZEnum $ for enumerators $ \ (Enumerator ident expr _ ni) →
						(ident2ast ident,eval_const_expr expr)
				where
				sueref = sueRef hassueref
				compkind2comptype StructTag = Struct
				compkind2comptype UnionTag  = Union

			modes = nub $ concat $ for (tyattrs++attrs) $ \case
				Attr (CIdent.Ident "mode" _ _) [CVar (CIdent.Ident mode _ _) _] _ → [mode]
				Attr (CIdent.Ident "fardata" _ _) _ _                             → []
				attr → error $ "mode: unknown attr " ++ show attr

		ArrayType elem_ty arraysize _ _ → ZArray (ty2ast [] elem_ty) $ case arraysize of
			ArraySize _ size   → Just $ eval_const_expr size
			UnknownArraySize _ → Nothing

		PtrType target_ty _ _ → ZPtr $ ty2ast [] target_ty

		FunctionType (FunType target_ty paramdecls is_variadic) _ →
			ZFun (ty2ast [] target_ty) is_variadic $ map ((ty2ast []).declType) paramdecls

		TypeDefType (TypeDefRef _ innerty _) _ attribs → ty2ast (attribs++attrs) innerty

		other → error $ "ty2ast " ++ show other ++ " not implemented!"

	decl2ast :: (Declaration d) => NodeInfo → d → VarDeclaration
	decl2ast ni decl = VarDeclaration (ident2ast ident) ((render.pretty) ty) (ty2ast attrs ty) (ni2loc ni)
		where
		VarDecl (VarName ident Nothing) (DeclAttrs _ _ attrs) ty = getVarDecl decl

	eval_const_expr :: CExpr -> Integer
	eval_const_expr (CConst (CIntConst cinteger _)) = getCInteger cinteger

--data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) Stmt) Loc
	identdecl2extdecl :: IdentDecl → ExtDecl

	identdecl2extdecl identdecl = ExtDecl vardeclast body loc
		where
		vardeclast = decl2ast ni identdecl
		ni = nodeInfo identdecl
		loc = ni2loc ni
		body = case identdecl of
			FunctionDef (FunDef vardecl stmt _)   → Right $ stmt2ast stmt
			Declaration (Decl vardecl _)          → Left Nothing
			EnumeratorDef (Enumerator _ expr _ _) → Left $ Just $ expr2ast expr
			ObjectDef (ObjDef vardecl mb_init _)  → Left $ fmap initializer2expr mb_init where
				initializer2expr :: CInitializer NodeInfo → Expr
				initializer2expr (CInitExpr expr _)     = expr2ast expr
				initializer2expr (CInitList initlist _) = Comp idexprs (typeVD vardeclast) loc where
					idexprs = for initlist $ \ ([],initializer) → initializer2expr initializer

	stmt2ast :: CStat → Stmt
	stmt2ast (CCompound _ cbis ni) = Compound (map stmt2ast cbis) (ni2loc ni)
	stmt2ast (CLabel ident stmt _ ni) = Label (ident2ast ident) (stmt2ast stmt) (ni2loc ni)
	stmt2ast (CIf expr then_stmt mb_else_stmt ni) =
		IfThenElse (expr2ast expr) (stmt2ast then_stmt) else_stmt (ni2loc ni) where
		else_stmt = case mb_else_stmt of
			Nothing     → Compound [] (ni2loc $ nodeInfo then_stmt)
			Just e_stmt → stmt2ast e_stmt
	stmt2ast (CExpr (Just expr) ni) = ExprStmt (expr2ast expr) (ni2loc ni)
	stmt2ast (CWhile cond body False ni) = Loop (expr2ast cond) (stmt2ast body) (ni2loc ni)
	stmt2ast (CWhile cond body True ni) = Compound [body',loop] (ni2loc ni) where
		body' = stmt2ast body
		loop = Loop (expr2ast cond) body' (ni2loc ni)
	stmt2ast (CFor mb_expr_or_decl mb_cond mb_inc body) =

	expr2ast :: CExpr → Expr
	expr2ast expr = Var (Ident "DUMMY" 0 (ni2loc $ nodeInfo expr)) ZUnit (ni2loc $ nodeInfo expr)
