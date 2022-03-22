{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,TupleSections,LambdaCase,UnicodeSyntax,PackageImports,StandaloneDeriving,DeriveGeneric #-}

module Parsing (
	parseFile)
	where

import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import Data.List (nub,sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as ASTMap

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

parseFile :: FilePath → R3 (Either String (TranslUnit TypeAttrs,TranslUnit ZType))
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
					return $ Right $ globDecls2AST machinespec deftable globaldecls

type TyEnvItem = (Ident,ZType)
type TyEnv = [TyEnvItem]

showTyEnv :: TyEnv -> String
showTyEnv tyenv = unlines $ map show tyenv

globDecls2AST :: MachineSpec → DefTable → GlobalDecls → (TranslUnit TypeAttrs,TranslUnit ZType)
globDecls2AST MachineSpec{..} deftable GlobalDecls{..} = (translunit_typeattrs,translsunit_ztype)
	where
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

	decl2stmt :: CDecl -> [Stmt TypeAttrs]
	decl2stmt (CDecl declspecs triples ni) = concatFor triples $
		\ (Just cdeclr@(CDeclr (Just cident) _ _ _ ni),mb_init,mb_size) → let
			ident = cident2ident cident
			ty = getCDeclType $ CDecl declspecs [(Just cdeclr,mb_init,mb_size)] ni
			tyattrs = Just (ty,[])
			init_assign = case mb_init of
				Nothing → []
				Just initializer → [ Var ident Nothing (ni2loc ni) ≔ initializer2expr tyattrs initializer ]
			in
			AST.Decl (VarDeclaration ident (renderpretty ty) tyattrs (ni2loc ni)) (ni2loc ni) : init_assign

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
			ObjectDef (ObjDef vardecl mb_init _)       → Left $ fmap (initializer2expr (typeVD vardeclast)) mb_init

	initializer2expr :: TypeAttrs → CInitializer NodeInfo → Expr TypeAttrs
	initializer2expr _       (CInitExpr expr _)      = expr2ast expr
	initializer2expr tyattrs (CInitList initlist ni) = Comp idexprs tyattrs (ni2loc ni) where
		idexprs = for initlist $ \ ([],initializer) → initializer2expr tyattrs initializer

	mb_compound_stmts :: [Stmt TypeAttrs] → Stmt TypeAttrs
	mb_compound_stmts [stmt] = stmt
	mb_compound_stmts stmts = Compound False stmts introLoc

	cbi2ast :: CBlockItem → Stmt TypeAttrs
	cbi2ast (CBlockStmt stmt) = stmt2ast stmt
	cbi2ast (CBlockDecl decl) = mb_compound_stmts $ decl2stmt decl

	-- makes a compound breakable
	mb_break_compound (CCompound _ cbis ni) = Compound True (map cbi2ast cbis) (ni2loc ni)
	mb_break_compound other = stmt2ast other

	emptyStmt :: Stmt TypeAttrs
	emptyStmt = Compound False [] introLoc

	stmt2ast :: CStat → Stmt TypeAttrs
	stmt2ast (CCompound _ cbis ni) = Compound False (map cbi2ast cbis) (ni2loc ni)
	stmt2ast (CLabel ident stmt _ ni) = Label (cident2ident ident) (stmt2ast stmt) (ni2loc ni)
	stmt2ast (CIf expr then_stmt mb_else_stmt ni) =
		IfThenElse (expr2ast expr) (stmt2ast then_stmt) else_stmt (ni2loc ni)
		where
		else_stmt = case mb_else_stmt of
			Nothing     → emptyStmt
			Just e_stmt → stmt2ast e_stmt
	stmt2ast (CExpr mb_expr ni) = case mb_expr of
		Just expr → ExprStmt (expr2ast expr) (ni2loc ni)
		Nothing   → emptyStmt
	stmt2ast (CWhile cond body is_dowhile ni) =
		(if is_dowhile then DoWhile else While) (expr2ast cond) (mb_break_compound body) (ni2loc ni)
	stmt2ast (CFor mb_expr_or_decl (Just cond) mb_inc body ni) =
		Compound False (inis ++ [ For (expr2ast cond) incs (mb_break_compound body) (ni2loc ni) ]) introLoc
		where
		inis = case mb_expr_or_decl of
			Left (Just ini_expr) → [ ExprStmt (expr2ast ini_expr) (ni2loc ini_expr) ]
			Right cdecl          → decl2stmt cdecl
		incs = case mb_inc of
			Nothing       → emptyStmt
			Just inc_expr → ExprStmt (expr2ast inc_expr) (ni2loc inc_expr)
	stmt2ast (CGoto cident ni) = Goto (cident2ident cident) (ni2loc ni)
	stmt2ast (CCont ni) = Continue (ni2loc ni)
	stmt2ast (CBreak ni) = Break (ni2loc ni)
	stmt2ast (CDefault stmt ni) = Default (stmt2ast stmt) (ni2loc ni)
	stmt2ast (CReturn mb_expr ni) = Return (fmap expr2ast mb_expr) (ni2loc ni)
	stmt2ast (CSwitch expr body ni) = Switch (expr2ast expr) (mb_break_compound body) (ni2loc ni)
	stmt2ast (CCase expr stmt ni) = Case (expr2ast expr) (stmt2ast stmt) (ni2loc ni)
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

	-----

	translsunit_ztype = map infer_extdecl translunit_typeattrs

	intTy  = ZInt intSize True :: ZType

	ty2zty :: (Type,Attributes) → ZType
	ty2zty (ty,attrs) = case ty of
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
			_                  → ZUnhandled $ renderpretty ty

			where

			resolve_sueref :: (HasSUERef a) => a → ZType
			resolve_sueref hassueref = case ASTMap.lookup sueref gTags of
				Nothing → error $ "Could not find " ++ show sueref ++ " in gTags"
				Just (CompDef (CompType sueref comptykind memberdecls attrs ni)) →
					ZCompound (renderpretty sueref) (compkind2comptype comptykind)
						(map (infer_vardecl . (decl2vardecl ni)) memberdecls)
				Just (EnumDef (EnumType sueref enumerators attrs ni)) →
					ZEnum (renderpretty sueref) $ for enumerators $ \ (Enumerator ident expr _ ni) →
						(cident2ident ident,eval_const_expr expr)
				where
				sueref = sueRef hassueref
				compkind2comptype StructTag = Struct
				compkind2comptype UnionTag  = Union

			modes = nub $ concat $ for (tyattrs++attrs) $ \case
				Attr (CIdent.Ident "mode" _ _) [CVar (CIdent.Ident mode _ _) _] _ → [mode]
				Attr (CIdent.Ident "fardata" _ _) _ _                             → []
				Attr (CIdent.Ident "__cdecl__" _ _) _ _                           → []
				attr → error $ "mode: unknown attr " ++ show attr

		ArrayType elem_ty arraysize _ attribs → ZArray (ty2zty (elem_ty,attribs++attrs)) $ case arraysize of
			ArraySize _ size   → Just $ eval_const_expr size
			UnknownArraySize _ → Nothing

		PtrType target_ty _ attribs → ZPtr $ ty2zty (target_ty,attribs++attrs)

		FunctionType (FunType target_ty paramdecls is_variadic) attribs →
			ZFun (ty2zty (target_ty,attribs++attrs)) is_variadic $
				map (ty2zty.(,attribs++attrs).declType) paramdecls

		TypeDefType (TypeDefRef _ innerty _) _ attribs → ty2zty (innerty,attribs++attrs)

		other → error $ "ty2zty " ++ show other ++ " not implemented!"

	eval_const_expr :: CExpr -> Integer
	eval_const_expr (CConst (CIntConst cinteger _)) = getCInteger cinteger

	infer_vardecl :: VarDeclaration TypeAttrs → VarDeclaration ZType
	infer_vardecl VarDeclaration{..} = VarDeclaration identVD sourceTypeVD (ty2zty tyattrs) locVD where
		Just tyattrs = typeVD

	vardecl2envitem :: VarDeclaration ZType → TyEnvItem
	vardecl2envitem VarDeclaration{..} = (identVD,typeVD)

	global_tyenv :: TyEnv
	global_tyenv = map (\ ExtDecl{..} → ( identVD varDeclED, ty2zty $ fromJust $ typeVD varDeclED )) translunit_typeattrs

	infer_extdecl :: ExtDecl TypeAttrs → ExtDecl ZType
	infer_extdecl (ExtDecl vardecl value loc) = ExtDecl vardecl' value' loc
		where
		vardecl' = infer_vardecl vardecl
		zty = typeVD vardecl'
		value' = case value of
			Left mb_expr → Left $ fmap (infer_expr [global_tyenv] (Just zty)) mb_expr
			Right (AST.FunDef vardecls body) → Right $ AST.FunDef (map infer_vardecl vardecls) body'
				where
				vardecls' = map infer_vardecl vardecls
				arg_env   = map vardecl2envitem vardecls'
				[body']   = infer_stmt [arg_env,global_tyenv] [body]
	mb_cast :: ZType -> Expr ZType -> Expr ZType
	mb_cast target_ty expr | target_ty /= typeE expr = Cast expr target_ty (NoLoc "<inserted>")
	mb_cast _ expr = expr

	lookupVarDeclsTy :: Ident → [VarDeclaration a] → a
	lookupVarDeclsTy ident vardecls = typeVD $ head $ filter ((==ident).identVD) vardecls

	infer_expr :: [TyEnv] → Maybe ZType → Expr TypeAttrs → Expr ZType
	infer_expr tyenvs mb_target_ty expr = case mb_target_ty of
		Nothing        → expr'
		Just target_ty → mb_cast target_ty expr'

		where

		τ      = infer_expr tyenvs Nothing
		τ_e ty = infer_expr tyenvs (Just ty)

		expr' = case expr of

			Assign lexpr expr Nothing loc →
				Assign lexpr' (τ_e lexpr'ty expr) lexpr'ty loc
				where
				lexpr'   = τ lexpr
				lexpr'ty = typeE lexpr'

			Cast expr (Just ty) loc → Cast (τ expr) (ty2zty ty) loc

			Call fun args Nothing loc → Call fun' (map (uncurry τ_e) $ zip argtys args) retty loc
				where
				fun' = τ fun
				ZFun retty _ argtys = typeE fun'

			Unary op expr Nothing loc → Unary op expr' ty loc where
				expr' = τ expr
				expr'_ty = typeE expr'
				ty = case op of
					AddrOf → ZPtr expr'_ty
					Deref  → targettyZ expr'_ty
					Not    → ZBool
					op | op `elem` [Plus,Minus,BitNeg,PreInc,PostInc,PreDec,PostDec]
					       → expr'_ty

			Binary op expr1 expr2 Nothing loc → Binary op (mb_cast arg_ty expr1') (mb_cast arg_ty expr2') res_ty loc
				where
				(expr1',expr2') = (τ expr1,τ expr2)
				max_ty = max (typeE expr1') (typeE expr2')
				(res_ty,arg_ty) = case op of
					op | op `elem` [Mul,Div,Add,Sub,Rmd,Shl,Shr,BitAnd,BitOr,BitXOr] → (max_ty,max_ty)
					op | op `elem` [Less,Equals,NotEquals,LessEq,Greater,GreaterEq]  → (ZBool,max_ty)
					op | op `elem` [And,Or]                                          → (ZBool,ZBool)
					other -> error $ "infer_expr " ++ show other ++ " not implemented"

			CondExpr cond then_expr else_expr Nothing loc →
				CondExpr cond' (mb_cast max_ty then_expr') (mb_cast max_ty else_expr') max_ty loc
				where
				cond' = τ_e ZBool cond
				(then_expr',else_expr') = (τ then_expr,τ else_expr)
				max_ty = max (typeE then_expr') (typeE else_expr')

			Index arr ix Nothing loc → Index arr' (τ_e intTy ix) elemty loc where
				arr' = τ arr
				ZArray elemty _ = typeE arr'

			Member expr member_ident isptr Nothing loc → Member expr' member_ident isptr ty loc
				where
				expr' = τ expr
				ty = lookupVarDeclsTy member_ident $ case (isptr,typeE expr') of
					(True ,ZPtr (ZCompound _ _ elemtys)) → elemtys
					(False,     (ZCompound _ _ elemtys)) → elemtys

			Var ident Nothing loc → Var ident ty loc
				where
				ty = case lookup ident (concat tyenvs) of
					Just t -> t
					Nothing -> error $ "lookup " ++ show ident ++ " not found in\n" ++ showTyEnv (concat tyenvs)

			Constant constant (Just ty) loc → Constant constant (ty2zty ty) loc

			Comp elems (Just ty) loc → Comp (map τ elems) (ty2zty ty) loc

			other → error $ "infer_expr " ++ show other ++ " not implemented"

	infer_stmt :: [TyEnv] → [Stmt TypeAttrs] → [Stmt ZType]
	infer_stmt _ [] = []
	infer_stmt tyenvs@(tyenv:resttyenvs) ((AST.Decl vardecl loc):stmts) =
		infer_stmt ((newenvitem:tyenv):resttyenvs) stmts
		where
		newenvitem = vardecl2envitem $ infer_vardecl vardecl

	infer_stmt tyenvs (stmt:stmts) = stmt' : infer_stmt tyenvs stmts
		where
		stmt' = case stmt of

			Compound catchbreak inner_stmts loc → Compound catchbreak (infer_stmt ([]:tyenvs) inner_stmts) loc

			IfThenElse cond then_stmt else_stmt loc → IfThenElse cond' then_stmt' else_stmt' loc
				where
				cond'        = infer_expr tyenvs (Just ZBool) cond
				[then_stmt'] = infer_stmt tyenvs [then_stmt]
				[else_stmt'] = infer_stmt tyenvs [else_stmt]

			ExprStmt expr loc → ExprStmt (infer_expr tyenvs Nothing expr) loc

			While cond body loc → While cond' body' loc where
				cond'   = infer_expr tyenvs (Just ZBool) cond
				[body'] = infer_stmt tyenvs [body]

			DoWhile cond body loc → DoWhile cond' body' loc where
				cond'   = infer_expr tyenvs (Just ZBool) cond
				[body'] = infer_stmt tyenvs [body]

			For cond inc body loc → For cond' inc' body' loc where
				cond'   = infer_expr tyenvs (Just ZBool) cond
				[inc']  = infer_stmt tyenvs [inc]
				[body'] = infer_stmt tyenvs [body]

			Switch val body loc → Switch (infer_expr tyenvs Nothing val) body' loc where
				[body']  = infer_stmt tyenvs [body]

			Return mb_expr loc → Return (fmap (infer_expr tyenvs Nothing) mb_expr) loc

			Default stmt loc → Default stmt' loc where
				[stmt'] = infer_stmt tyenvs [stmt]

			Case val stmt loc → Case (infer_expr tyenvs Nothing val) stmt' loc where
				[stmt'] = infer_stmt tyenvs [stmt]

			Continue loc → Continue loc

			Break loc → Break loc

			other → error $ "infer_stmt " ++ show other ++ " not implemented"
