{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,TupleSections,LambdaCase,UnicodeSyntax,PackageImports,StandaloneDeriving,DeriveGeneric #-}

module Parsing (
	parseFile)
	where

import Language.C (parseCFile,CDecl,pretty)
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.SemRep as SemRep hiding (Stmt,Expr)
import Language.C.Analysis.DefTable (DefTable)
import Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_,getDefTable,withDefTable)
import qualified Language.C.Data.Ident as CIdent
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (CInteger(..),getCInteger,CFloat(..),getCString,getCChar)
import Language.C.Data.Node (lengthOfNode,NodeInfo,nodeInfo,CNode)
import Language.C.Data.Position (posOf,posFile,posRow,posColumn,isSourcePos)
import Language.C.Syntax.Ops (assignBinop)
import Language.C.Analysis.TypeUtils (getIntType,getFloatType,integral,floating)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import Data.List (nub,sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as ASTMap

import qualified Text.PrettyPrint as TextPretty
import qualified Language.C.Pretty as LangCPretty
import Prettyprinter

import GlobDecls
import AST
import R3Monad
import MachineSpec
import Utils
import DataTree

import GHC.Generics
deriving instance Generic Type
deriving instance Generic TypeName
deriving instance Generic TypeQuals
deriving instance Generic Attr
deriving instance Generic ArraySize
deriving instance Generic FunType
deriving instance Generic TypeDefRef
deriving instance Generic IntType
deriving instance Generic FloatType
deriving instance Generic CompTypeRef
deriving instance Generic EnumTypeRef
deriving instance Generic BuiltinType
deriving instance Generic CompTyKind
deriving instance Generic ParamDecl
deriving instance Generic VarDecl
deriving instance Generic DeclAttrs
deriving instance Generic FunctionAttrs
deriving instance Generic Storage
deriving instance Generic Linkage
deriving instance Generic VarName


type TypeAttrs = Maybe (Type,Attributes)

renderpretty ::(LangCPretty.Pretty a) => a -> String
renderpretty a = TextPretty.render $ LangCPretty.pretty $ a

instance Pretty Type where
	pretty ty = Prettyprinter.pretty $ renderpretty ty
instance Pretty Attr where
	pretty attr = Prettyprinter.pretty $ renderpretty attr

parseFile :: FilePath → R3 (Either String (TranslUnit TypeAttrs))
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
				Left errs → return $ Left $ "HARD ERRORS:\n" ++ unlines (map show errs)
				Right ((globaldecls,deftable),_) → do
					liftIO $ writeFile "GlobDecls.html" $ globdeclsToHTMLString globaldecls
					machinespec <- getMachineSpec gcc
					let (ast_typeattr,ast) = globDecls2AST machinespec deftable globaldecls
					liftIO $ writeFile "AST.html" $ astToHTMLString ast
					liftIO $ writeFile "translunit.TypeAttr" $ prettyTranslUnitString ast_typeattr
					return $ Right ast

type TyEnvItem = (Ident,ZType)
type TyEnv = [TyEnvItem]

showTyEnv :: TyEnv -> String
showTyEnv tyenv = unlines $ map show tyenv

globDecls2AST :: MachineSpec → DefTable → GlobalDecls → (TranslUnit TypeAttrs,TranslUnit ZType)
globDecls2AST MachineSpec{..} deftable GlobalDecls{..} = (translunit_typeattrs,translunit_ztype)

	where

	translunit_typeattrs :: TranslUnit TypeAttrs
	translunit_typeattrs = sortBy (comparing locED) $ map identdecl2extdecl $ ASTMap.elems gObjs

	ni2loc :: (CNode n) => n → Loc
	ni2loc n = let ni = nodeInfo n in case posOf ni of
		pos | isSourcePos pos → Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
		other                 → NoLoc $ show other

	cident2ident :: CIdent.Ident → Ident
	cident2ident (CIdent.Ident name i ni) = Ident name i (ni2loc ni)

	getCDeclType :: CDecl -> Type
	getCDeclType cdecl = case runTrav_ (withDefTable (const ((),deftable)) >> analyseTypeDecl cdecl) of
		Left errs    → error $ show errs
		Right (ty,_) → ty

	decl2stmt :: CDecl -> Stmt TypeAttrs
	decl2stmt (CDecl declspecs triples ni) = Decls vardecls (ni2loc ni)
		where
		vardecls = for triples $ \ (Just cdeclr@(CDeclr (Just cident) _ _ _ ni),mb_init,mb_expr) →
			let ty = getCDeclType $ CDecl declspecs [(Just cdeclr,mb_init,mb_expr)] ni in
				VarDeclaration (cident2ident cident) (renderpretty ty) (Just (ty,[])) (ni2loc ni)

	decl2vardecl :: (Declaration d) => NodeInfo → d → VarDeclaration TypeAttrs
	decl2vardecl ni decl = VarDeclaration (cident2ident ident) (renderpretty ty) (Just (ty,attrs)) (ni2loc ni)
		where
		VarDecl (VarName ident Nothing) (DeclAttrs _ _ attrs) ty = getVarDecl decl

	identdecl2extdecl :: IdentDecl → ExtDecl TypeAttrs
	identdecl2extdecl identdecl = ExtDecl vardeclast body loc
		where
		vardeclast = decl2vardecl ni identdecl
		ni = nodeInfo identdecl
		loc = ni2loc ni
		body = case identdecl of
			FunctionDef (SemRep.FunDef (VarDecl _ _ (FunctionType (FunType _ paramdecls _) _)) stmt ni) →
				Right $ AST.FunDef (map (decl2vardecl ni) paramdecls) (stmt2ast stmt)
			SemRep.Declaration (SemRep.Decl vardecl _) → Left Nothing
			EnumeratorDef (Enumerator _ expr _ _)      → Left $ Just $ expr2ast expr
			ObjectDef (ObjDef vardecl mb_init _)       → Left $ fmap initializer2expr mb_init where
				initializer2expr :: CInitializer NodeInfo → Expr TypeAttrs
				initializer2expr (CInitExpr expr _)     = expr2ast expr
				initializer2expr (CInitList initlist _) = Comp idexprs (typeVD vardeclast) loc where
					idexprs = for initlist $ \ ([],initializer) → initializer2expr initializer

	cbi2ast :: CBlockItem -> Stmt TypeAttrs
	cbi2ast (CBlockStmt stmt) = stmt2ast stmt
	cbi2ast (CBlockDecl decl) = decl2stmt decl

	stmt2ast :: CStat → Stmt TypeAttrs
	stmt2ast (CCompound _ cbis ni) = Compound (map cbi2ast cbis) (ni2loc ni)
	stmt2ast (CLabel ident stmt _ ni) = Label (cident2ident ident) (stmt2ast stmt) (ni2loc ni)
	stmt2ast (CIf expr then_stmt mb_else_stmt ni) =
		IfThenElse (expr2ast expr) (stmt2ast then_stmt) else_stmt (ni2loc ni) where
		else_stmt = case mb_else_stmt of
			Nothing     → Compound [] (ni2loc then_stmt)
			Just e_stmt → stmt2ast e_stmt
	stmt2ast (CExpr mb_expr ni) = case mb_expr of
		Just expr -> ExprStmt (expr2ast expr) (ni2loc ni)
		Nothing   -> Compound [] (ni2loc ni)
	stmt2ast (CWhile cond body is_dowhile ni) =
		(if is_dowhile then DoWhile else While) (expr2ast cond) (stmt2ast body) (ni2loc ni)
	stmt2ast (CFor mb_expr_or_decl (Just cond) mb_inc body ni) =
		For ini (expr2ast cond) inc (stmt2ast body) (ni2loc ni)
		where
		ini = case mb_expr_or_decl of
			Left (Just ini_expr) → ExprStmt (expr2ast ini_expr) (ni2loc ini_expr)
			Right cdecl          → decl2stmt cdecl
		inc = case mb_inc of
			Nothing       → Compound [] (ni2loc ni)
			Just inc_expr → ExprStmt (expr2ast inc_expr) (ni2loc inc_expr)
	stmt2ast (CGoto cident ni) = Goto (cident2ident cident) (ni2loc ni)
	stmt2ast (CCont ni) = Continue (ni2loc ni)
	stmt2ast (CBreak ni) = Break (ni2loc ni)
	stmt2ast (CReturn mb_expr ni) = Return (fmap expr2ast mb_expr) (ni2loc ni)
	stmt2ast (CSwitch expr body ni) = Switch (expr2ast expr) (stmt2ast body) (ni2loc ni)
	stmt2ast other = error $ "stmt2ast " ++ show other ++ " not implemented"

	expr2ast :: CExpr → Expr TypeAttrs
	expr2ast (CAssign ass_op lexpr rexpr ni) = Assign lexpr' expr' Nothing (ni2loc ni) where
		lexpr' = expr2ast lexpr
		rexpr' = expr2ast rexpr
		expr'  = case ass_op of
			CAssignOp → rexpr'
			other_op  → Binary binop lexpr' rexpr' Nothing (ni2loc rexpr) where
				Just binop = lookup ass_op [
					(CMulAssOp,Mul),(CDivAssOp,Div),(CRmdAssOp,Rmd),(CAddAssOp,Add),(CSubAssOp,Sub),
					(CShlAssOp,Shl),(CShrAssOp,Shr),(CAndAssOp,BitAnd),(CXorAssOp,BitXOr),(COrAssOp,BitOr) ]
	expr2ast (CCond cond (Just then_expr) else_expr ni) =
		CondExpr (expr2ast cond) (expr2ast then_expr) (expr2ast else_expr) Nothing (ni2loc ni)
	expr2ast (CCast cdecl expr ni) = Cast (expr2ast expr) (Just (getCDeclType cdecl,[])) (ni2loc ni)
	expr2ast (CBinary binop expr1 expr2 ni) = Binary binop' (expr2ast expr1) (expr2ast expr2) Nothing (ni2loc ni) where
		Just binop' = lookup binop [
			(CMulOp,Mul),(CDivOp,Div),(CRmdOp,Rmd),(CAddOp,Add),(CSubOp,Sub),(CShlOp,Shl),
			(CShrOp,Shr),(CLeOp,LessEq),(CGrOp,Greater),(CLeqOp,LessEq),(CGeqOp,GreaterEq),(CEqOp,Equals),(CNeqOp,NotEquals),
			(CAndOp,BitAnd),(CXorOp,BitXOr),(COrOp,BitOr),(CLndOp,And),(CLorOp,Or) ]
	expr2ast (CUnary unop expr ni) = Unary unop' (expr2ast expr) Nothing (ni2loc ni) where
		Just unop' = lookup unop [
			(CPreIncOp,PreInc),(CPreDecOp,PreDec),(CPostIncOp,PostInc),(CPostDecOp,PostDec),(CAdrOp,AddrOf),
			(CIndOp,Deref),(CPlusOp,Plus),(CMinOp,Minus),(CCompOp,BitNeg),(CNegOp,Not) ]
	expr2ast (CIndex arr index ni) = Index (expr2ast arr) (expr2ast index) Nothing (ni2loc ni)
	expr2ast (CConst ctconst) = Constant const' (Just ty) (ni2loc ni) where
		(const',ty,ni) = case ctconst of
			CIntConst cinteger@(CInteger _ _ flags) ni → (IntConst (getCInteger cinteger),(integral $ getIntType flags,[]),ni)
			CCharConst cchar ni       → (CharConst (read $ getCChar cchar),(integral TyChar,[]),ni)
			CFloatConst (CFloat f) ni → (FloatConst (read f),(floating $ getFloatType f,[]),ni)
			CStrConst cstring ni      → (StringConst $ (getCString cstring),(PtrType (integral TyChar) noTypeQuals noAttributes,[]),ni)
	expr2ast (CMember expr ident is_ptr ni) = Member (expr2ast expr) (cident2ident ident) is_ptr Nothing (ni2loc ni)
	expr2ast (CVar ident ni) = Var (cident2ident ident) Nothing (ni2loc ni)
	expr2ast (CCall fun args ni) = Call (expr2ast fun) (map expr2ast args) Nothing (ni2loc ni)
	expr2ast other = error $ "expr2ast " ++ show other ++ " not implemented"

	intType = Just (integral TyInt,[]) :: TypeAttrs

