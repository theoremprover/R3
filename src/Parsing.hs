{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,LambdaCase,UnicodeSyntax,PackageImports,StandaloneDeriving,DeriveGeneric #-}

module Parsing (
	parseFile)
	where

import Language.C (parseCFile,CDecl,pretty)
--import "language-c" Language.C (parseCFile,CDecl,pretty)
import Language.C.System.GCC (newGCC)
import Language.C.Analysis.SemRep as SemRep hiding (Stmt,Expr)
import Language.C.Analysis.DefTable (DefTable)
import Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import Language.C.Analysis.AstAnalysis (analyseAST)
import Language.C.Analysis.TravMonad (runTrav_,getDefTable,withDefTable)
import qualified Language.C.Data.Ident as CIdent
import Language.C.Syntax.AST
import Language.C.Syntax.Constants (getCInteger,CFloat(..),getCString,getCChar)
import Language.C.Data.Node (lengthOfNode,NodeInfo,nodeInfo,CNode)
import Language.C.Data.Position (posOf,posFile,posRow,posColumn,isSourcePos)
import Language.C.Syntax.Ops (assignBinop)
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
				Left errs -> return $ Left $ "HARD ERRORS:\n" ++ unlines (map show errs)
				Right ((globaldecls,deftable),_) → do
					liftIO $ writeFile "GlobDecls.html" $ globdeclsToHTMLString globaldecls
					machinespec <- getMachineSpec gcc
					let ast = globDecls2AST machinespec deftable globaldecls
					liftIO $ writeFile "AST.html" $ astToHTMLString ast
					return $ Right ast

type TypeAttrs = Maybe (Type,Attributes)

globDecls2AST :: MachineSpec → DefTable → GlobalDecls → TranslUnit TypeAttrs
globDecls2AST MachineSpec{..} deftable GlobalDecls{..} = ASTMap.map identdecl2extdecl $ ASTMap.mapKeys cident2ident gObjs
	where

	ni2loc :: (CNode n) => n → Loc
	ni2loc n = let ni = nodeInfo n in case posOf ni of
		pos | isSourcePos pos → Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
		other                 → NoLoc $ show other

	cident2ident :: CIdent.Ident → Ident
	cident2ident (CIdent.Ident name i ni) = Ident name i (ni2loc ni)

	getCDeclType :: CDecl -> Type
	getCDeclType cdecl = case runTrav_ (withDefTable (const ((),deftable)) >> analyseTypeDecl cdecl) of
		Left errs    -> error $ show errs
		Right (ty,_) -> ty

	decl2stmt :: CDecl -> Stmt TypeAttrs
	decl2stmt (CDecl declspecs triples ni) = Decls vardecls (ni2loc ni)
		where
		vardecls = for triples $ \ (Just cdeclr@(CDeclr (Just cident) _ _ _ ni),mb_init,mb_expr) ->
			let ty = getCDeclType $ CDecl declspecs [(Just cdeclr,mb_init,mb_expr)] ni in
				VarDeclaration (cident2ident cident) ((render.pretty) ty) (Just (ty,[])) (ni2loc ni)
{-
		withDefTable (const ((),deftable))
		ty <- analyseTypeDecl $ CDecl declspecs [(Just cdeclr,mb_init,mb_expr)] ni
		return (cident,ty,ni)
		of
		Left errs                -> error $ show errs
		Right ((cident,ty,ni),_) ->
			VarDeclaration
-}
{-
	intTy = ZInt intSize True

	ty2zty :: Attributes → Type → ZType
	ty2zty attrs ty = case ty of
		DirectType tyname _ tyattrs → case tyname of
			TyIntegral intty → case (intty,modes) of
				(TyChar,[])     → ZInt  8 True
				(TySChar,[])    → ZInt  8 False
				(TyUChar,[])    → ZInt  8 True
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
				other           → error $ "ty2zty " ++ show other ++ " not implemented!"
			TyFloating floatty → case (floatty,modes) of
				(TyFloat,[])     → ZFloat  8  24
				(TyFloat,["SF"]) → ZFloat  8  24
				(TyFloat,["DF"]) → ZFloat 11  53
				(TyDouble,[])    → ZFloat 11  53
				(TyLDouble,[])   → ZFloat 15 113
				other            → error $ "ty2zty " ++ show other ++ " not implemented!"
			TyVoid             → ZUnit
			TyEnum enumtyperef → resolve_sueref enumtyperef
			TyComp comptyperef → resolve_sueref comptyperef
			_                  → ZUnhandled $ (render.pretty) ty

			where

			resolve_sueref :: (HasSUERef a) => a → ZType
			resolve_sueref hassueref = case ASTMap.lookup sueref gTags of
				Nothing → error $ "Could not find " ++ show sueref ++ " in gTags"
				Just (CompDef (CompType _ comptykind memberdecls attrs ni)) →
					ZCompound (compkind2comptype comptykind) (map (decl2vardecl ni) memberdecls)
				Just (EnumDef (EnumType _ enumerators attrs ni)) →
					ZEnum $ for enumerators $ \ (Enumerator ident expr _ ni) →
						(cident2ident ident,eval_const_expr expr)
				where
				sueref = sueRef hassueref
				compkind2comptype StructTag = Struct
				compkind2comptype UnionTag  = Union

			modes = nub $ concat $ for (tyattrs++attrs) $ \case
				Attr (CIdent.Ident "mode" _ _) [CVar (CIdent.Ident mode _ _) _] _ → [mode]
				Attr (CIdent.Ident "fardata" _ _) _ _                             → []
				attr → error $ "mode: unknown attr " ++ show attr

		ArrayType elem_ty arraysize _ _ → ZArray (ty2zty [] elem_ty) $ case arraysize of
			ArraySize _ size   → Just $ eval_const_expr size
			UnknownArraySize _ → Nothing

		PtrType target_ty _ _ → ZPtr $ ty2zty [] target_ty

		FunctionType (FunType target_ty paramdecls is_variadic) _ →
			ZFun (ty2zty [] target_ty) is_variadic $ map ((ty2zty []).declType) paramdecls

		TypeDefType (TypeDefRef _ innerty _) _ attribs → ty2zty (attribs++attrs) innerty

		other → error $ "ty2zty " ++ show other ++ " not implemented!"

	eval_const_expr :: CExpr -> Integer
	eval_const_expr (CConst (CIntConst cinteger _)) = getCInteger cinteger
-}

	decl2vardecl :: (Declaration d) => NodeInfo → d → VarDeclaration TypeAttrs
	decl2vardecl ni decl = VarDeclaration (cident2ident ident) ((render.pretty) ty) (Just (ty,attrs)) (ni2loc ni)
		where
		VarDecl (VarName ident Nothing) (DeclAttrs _ _ attrs) ty = getVarDecl decl

	identdecl2extdecl :: IdentDecl → ExtDecl TypeAttrs
	identdecl2extdecl identdecl = ExtDecl vardeclast body loc
		where
		vardeclast = decl2vardecl ni identdecl
		ni = nodeInfo identdecl
		loc = ni2loc ni
		body = case identdecl of
			FunctionDef (FunDef vardecl stmt _)   → Right $ stmt2ast stmt
			SemRep.Declaration (SemRep.Decl vardecl _)   → Left Nothing
			EnumeratorDef (Enumerator _ expr _ _) → Left $ Just $ expr2ast expr
			ObjectDef (ObjDef vardecl mb_init _)  → Left $ fmap initializer2expr mb_init where
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
	stmt2ast (CExpr (Just expr) ni) = ExprStmt (expr2ast expr) (ni2loc ni)
	stmt2ast (CWhile cond body False ni) = While (expr2ast cond) (stmt2ast body) (ni2loc ni)
	stmt2ast (CWhile cond body True ni) = Compound [body',loop] (ni2loc ni) where
		body' = stmt2ast body
		loop = While (expr2ast cond) body' (ni2loc ni)
	stmt2ast (CFor mb_expr_or_decl (Just cond) mb_inc body ni) = Compound [ini,loop] (ni2loc ni) where
		ini = case mb_expr_or_decl of
			Left (Just ini_expr) → ExprStmt (expr2ast ini_expr) (ni2loc ini_expr)
			Right cdecl          → decl2stmt cdecl
		loop = While (expr2ast cond) body' (ni2loc ni)
		body' = Compound [stmt2ast body,inc] (ni2loc body)
		inc = case mb_inc of
			Nothing       → Compound [] (ni2loc ni)
			Just inc_expr → ExprStmt (expr2ast inc_expr) (ni2loc inc_expr)
	stmt2ast (CGoto cident ni) = Goto (cident2ident cident) (ni2loc ni)
	stmt2ast (CCont ni) = Continue (ni2loc ni)
	stmt2ast (CBreak ni) = Break (ni2loc ni)
	stmt2ast (CReturn mb_expr ni) = Return (fmap expr2ast mb_expr) (ni2loc ni)
--	stmt2ast (CSwitch expr (CCompound [] cbis _) ni) =
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
			(CIndOp,Deref),(CPlusOp,Plus),(CMinOp,Minus),(CCompOp,ExOr),(CNegOp,Not) ] :: Maybe UnaryOp
	expr2ast (CIndex arr index ni) = Index (expr2ast arr) (expr2ast index) Nothing (ni2loc ni)
	expr2ast (CConst const) = Constant const' Nothing (ni2loc ni) where
		(const',ni) = case const of
			CIntConst cinteger ni     -> (IntConst (getCInteger cinteger),ni)
			CCharConst cchar ni       -> (CharConst (read $ getCChar cchar),ni)
			CFloatConst (CFloat f) ni -> (FloatConst (read f),ni)
			CStrConst cstring ni      -> (StringConst $ (getCString cstring),ni)
	expr2ast (CMember expr ident is_ptr ni) = Member (expr2ast expr) (cident2ident ident) is_ptr Nothing (ni2loc ni)
	expr2ast (CVar ident ni) = Var (cident2ident ident) Nothing (ni2loc ni)
	expr2ast (CCall fun args ni) = Call (expr2ast fun) (map expr2ast args) Nothing (ni2loc ni)
	expr2ast other = error $ "expr2ast " ++ show other ++ " not implemented"
