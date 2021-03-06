{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,TypeSynonymInstances,FlexibleInstances,TupleSections,LambdaCase,UnicodeSyntax,PackageImports,StandaloneDeriving,DeriveGeneric #-}

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


renderpretty ::(LangCPretty.Pretty a) => a -> String
renderpretty a = TextPretty.render $ LangCPretty.pretty $ a

instance Pretty Type where
	pretty ty = Prettyprinter.pretty $ renderpretty ty

parseFile :: FilePath ??? R3 (Either String TranslUnit)
parseFile filepath = do
	gcc <- gets compilerR3
	liftIO $ parseCFile (newGCC gcc) Nothing [] filepath >>= \case
		Left parseerror   ??? return $ Left $ show parseerror
		Right ctranslunit ??? do
			writeFile "LanguageC_AST.html" $ genericToHTMLString ctranslunit
			case runTrav_ $ do
				globaldecls <- analyseAST ctranslunit
				deftable <- getDefTable
				return (globaldecls,deftable)
				of
				Left errs ??? return $ Left $ "HARD ERRORS:\n" ++ unlines (map show errs)
				Right ((globaldecls,deftable),_) ??? do
					liftIO $ writeFile "GlobDecls.html" $ globdeclsToHTMLString globaldecls
					machinespec <- getMachineSpec gcc
					return $ Right $ globDecls2AST machinespec deftable globaldecls

globDecls2AST :: MachineSpec ??? DefTable ??? GlobalDecls ??? TranslUnit
globDecls2AST MachineSpec{..} deftable GlobalDecls{..} =
	sortBy (comparing locED) $ map identdecl2extdecl $ ASTMap.elems gObjs

	where

	global_?? :: TyEnv
	global_?? = for (ASTMap.assocs gObjs) $ \ (ident,decl) ??? ( cident2ident ident, decl2zty decl )

	decl2zty decl = ty2zty (declType decl,attrs) where
		DeclAttrs _ _ attrs = declAttrs decl

	ni2loc :: (CNode n) => n ??? Loc
	ni2loc n = let ni = nodeInfo n in case posOf ni of
		pos | isSourcePos pos ??? Loc (posFile pos) (posRow pos) (posColumn pos) (fromJust $ lengthOfNode ni)
		other                 ??? NoLoc $ show other

	cident2ident :: CIdent.Ident ??? Ident
	cident2ident (CIdent.Ident name i ni) = Ident name i (ni2loc ni)

	cboolTy :: ZType
	cboolTy = ZInt intSize False

	indexTy :: ZType
	indexTy = ZInt intSize True

	ty2zty :: (Type,Attributes) ??? ZType
	ty2zty (ty,attrs) = case ty of
		DirectType tyname _ tyattrs ??? case tyname of
			TyIntegral intty ??? case (intty,modes) of
				(TyChar,[])     ??? ZInt  8 True
				(TySChar,[])    ??? ZInt  8 False
				(TyUChar,[])    ??? ZInt  8 True
				(TyShort,[])    ??? ZInt 16 False
				(TyUShort,[])   ??? ZInt 16 True
				(TyInt,[])      ??? ZInt intSize False
				(TyInt,["SI"])  ??? ZInt 32 False
				(TyInt,["DI"])  ??? ZInt 64 False
				(TyUInt,[])     ??? ZInt intSize True
				(TyUInt,["SI"]) ??? ZInt 32 True
				(TyUInt,["DI"]) ??? ZInt 64 True
				(TyLong,[])     ??? ZInt longSize False
				(TyULong,[])    ??? ZInt longSize True
				(TyLLong,[])    ??? ZInt longLongSize False
				(TyULLong,[])   ??? ZInt longLongSize True
				other           ??? error $ "ty2zty " ++ show other ++ " not implemented!"
			TyFloating floatty ??? case (floatty,modes) of
				(TyFloat,[])     ??? ZFloat  8  24
				(TyFloat,["SF"]) ??? ZFloat  8  24
				(TyFloat,["DF"]) ??? ZFloat 11  53
				(TyDouble,[])    ??? ZFloat 11  53
				(TyLDouble,[])   ??? ZFloat 15 113
				other            ??? error $ "ty2zty " ++ show other ++ " not implemented!"
			TyVoid             ??? ZUnit
			TyEnum enumtyperef ??? resolve_sueref enumtyperef
			TyComp comptyperef ??? resolve_sueref comptyperef
			_                  ??? ZUnhandled $ renderpretty ty

			where

			resolve_sueref :: (HasSUERef a) => a ??? ZType
			resolve_sueref hassueref = case ASTMap.lookup sueref gTags of
				Nothing ??? error $ "Could not find " ++ show sueref ++ " in gTags"
				Just (CompDef (CompType sueref comptykind memberdecls attrs _)) ???
					ZCompound (renderpretty sueref) (compkind2comptype comptykind)
						(map decl2vardecl memberdecls)
				Just (EnumDef (EnumType sueref enumerators attrs ni)) ???
					ZEnum (renderpretty sueref) $ for enumerators $ \ (Enumerator ident expr _ ni) ???
						(cident2ident ident,eval_const_expr expr)
				where
				sueref = sueRef hassueref
				compkind2comptype StructTag = Struct
				compkind2comptype UnionTag  = Union

			modes = nub $ concat $ for (tyattrs++attrs) $ \case
				Attr (CIdent.Ident "mode" _ _) [CVar (CIdent.Ident mode _ _) _] _ ??? [mode]
				Attr (CIdent.Ident "fardata" _ _) _ _                             ??? []
				Attr (CIdent.Ident "__cdecl__" _ _) _ _                           ??? []
				attr ??? error $ "mode: unknown attr " ++ show attr

		FunctionType (FunType ret_ty paramdecls isvariadic) _ ???
			ZFun (ty2zty (ret_ty,noAttributes)) isvariadic (map decl2zty paramdecls)

		PtrType ty _ attrs ??? ZPtr (ty2zty (ty,attrs))

		ArrayType ty arr_size _ attrs ??? ZArray (ty2zty (ty,attrs)) $ case arr_size of
			UnknownArraySize _ ??? Nothing
			ArraySize _ expr   ??? Just $ eval_const_expr expr

		TypeDefType (TypeDefRef _ ty _) _ attrs ??? ty2zty (ty,attrs)

		other ??? error $ "ty2zty " ++ show other ++ " not implemented"

		where

		eval_const_expr :: CExpr -> Integer
		eval_const_expr (CConst (CIntConst cinteger _)) = getCInteger cinteger

	decl2vardecl :: (CNode d,Declaration d) => d ??? VarDeclaration
	decl2vardecl decl = VarDeclaration (cident2ident $ identOfVarName varname) (renderpretty ty) zty (ni2loc decl)
		where
		VarDecl varname _ ty = getVarDecl decl
		zty = decl2zty decl

	vardecl2tyenvitem :: VarDeclaration ??? TyEnvItem
	vardecl2tyenvitem VarDeclaration{..} = (identVD,typeVD)

	identdecl2extdecl :: IdentDecl ??? ExtDecl
	identdecl2extdecl identdecl = ExtDecl vardecl body (ni2loc identdecl)
		where
		vardecl@VarDeclaration{..} = decl2vardecl identdecl
		body = case identdecl of
			FunctionDef (SemRep.FunDef (VarDecl _ _ (FunctionType (FunType rty paramdecls _) _)) stmt ni) ???
				Right $ AST.FunDef paramvardecls (mb_compound_stmts $ stmt2ast fun_?? (ty2zty (rty,[])) stmt)
				where
				paramvardecls = map decl2vardecl paramdecls
				fun_?? = map vardecl2tyenvitem paramvardecls ++ global_??
			SemRep.Declaration _                  ??? Left Nothing
			EnumeratorDef (Enumerator _ expr _ _) ??? Left $ Just $ expr2ast global_?? (Just typeVD) expr
			ObjectDef (ObjDef _ mb_init _)        ??? Left $ fmap (initializer2expr global_?? typeVD) mb_init

	initializer2expr :: TyEnv ??? ZType ??? CInitializer NodeInfo ??? Expr
	initializer2expr ?? ty (CInitExpr cexpr _)     = expr2ast ?? (Just ty) cexpr
	initializer2expr ?? ty cinitlist@(CInitList initlist ni) = Comp idexprs ty (ni2loc ni)
		where
		tylist = case ty of
			ZArray elem_ty _            ??? repeat elem_ty
			ZCompound _ _ comp_vardecls ??? map typeVD comp_vardecls
		inits = case length initlist == length tylist of
			False ??? error $ "initializer2expr: length initlist /= length tylist at " ++ show ni
			True  ??? zip initlist tylist
		idexprs = for inits $ \ (([],cinitializer),ty) ??? initializer2expr ?? ty cinitializer

	mb_compound_stmts :: [Stmt] ??? Stmt
	mb_compound_stmts [stmt] = stmt
	mb_compound_stmts stmts = Compound False stmts introLoc

-- ??????????????????????????????????????????????????

	getCDeclType :: CDecl -> Type
	getCDeclType cdecl = case runTrav_ (withDefTable (const ((),deftable)) >> analyseTypeDecl cdecl) of
		Left errs    ??? error $ show errs
		Right (ty,_) ??? ty

	stmt2ast :: TyEnv ??? ZType ??? CStat ??? [Stmt]
	stmt2ast ?? ret_ty cstat = case cstat of

		CCompound _ cbis _    ??? [ Compound False (cbis2ast ?? cbis) loc ]

		CLabel ident stmt _ _ ??? Label (cident2ident ident) loc : stmt2ast ?? ret_ty stmt

		CIf expr then_stmt mb_else_stmt _ ???
			[ IfThenElse (expr2ast ?? (Just cboolTy) expr) (mb_compound_stmts $ stmt2ast ?? ret_ty then_stmt) else_stmt loc ]
			where
			else_stmt = case mb_else_stmt of
				Nothing     ??? emptyStmt
				Just e_stmt ??? mb_compound_stmts $ stmt2ast ?? ret_ty e_stmt

		CExpr mb_expr _ ??? case mb_expr of
			Just expr ??? [ ExprStmt (expr2ast ?? Nothing expr) loc ]
			Nothing   ??? []

		CWhile cond body is_do_while ni ??? [ While is_do_while
			(expr2ast ?? (Just cboolTy) cond) (mb_break_compound $ stmt2ast ?? ret_ty body) loc ]

		CFor mb_expr_or_decl (Just cond) mb_inc body ni ???
			[ mb_compound_stmts $ inis ++ [ While False (expr2ast ??' (Just cboolTy) cond) (mb_break_compound $ stmt2ast ??' ret_ty body ++ [inc]) loc ] ]
			where
			(??',inis) = case mb_expr_or_decl of
				Left (Just ini_expr) ??? ( ??, [ ExprStmt (expr2ast ?? Nothing ini_expr) (ni2loc ini_expr) ] )
				Right cdecl          ??? cdecl2stmts ?? cdecl
			inc = case mb_inc of
				Nothing       ??? emptyStmt
				Just inc_expr ??? ExprStmt (expr2ast ??' Nothing inc_expr) (ni2loc inc_expr)

		CGoto cident ni ??? [ Goto (cident2ident cident) loc ]

		CCont _ ??? [ Continue loc ]

		CReturn mb_expr _ ??? [ Return (fmap (expr2ast ?? (Just ret_ty)) mb_expr) loc ]

		CDefault cstmt _ ??? Default loc : stmt2ast ?? ret_ty cstmt

		CCase expr stmt _ ??? Case (expr2ast ?? Nothing expr) loc : stmt2ast ?? ret_ty stmt

		CSwitch expr body _ ??? [ Switch (expr2ast ?? Nothing expr)
			(mb_break_compound $ stmt2ast ?? ret_ty body) loc ]

		CBreak _ ??? [ Break loc ]

		other ??? error $ "stmt2ast " ++ show other ++ " not implemented"

		where

		loc = ni2loc cstat

		-- makes a breakable compound
		mb_break_compound :: [Stmt] -> Stmt
		mb_break_compound [Compound _ stmts loc] = Compound True stmts loc
		mb_break_compound stmts = Compound True stmts introLoc

		cbis2ast :: TyEnv ??? [CBlockItem] ??? [Stmt]
		cbis2ast ?? [] = []
		cbis2ast ?? (CBlockStmt stmt : cbis)  = stmt2ast ?? ret_ty stmt ++ cbis2ast ?? cbis
		cbis2ast ?? (CBlockDecl cdecl : cbis) = declstmts ++ cbis2ast ??' cbis where
			(??',declstmts) = cdecl2stmts ?? cdecl

		cdecl2stmts :: TyEnv ??? CDecl -> (TyEnv,[Stmt])
		cdecl2stmts ?? (CDecl declspecs triples ni) = triples2stmts ?? [] triples
			where
			triples2stmts ?? acc [] = (??,acc)
			triples2stmts ?? acc ((Just cdeclr@(CDeclr (Just cident) _ _ _ ni),mb_init,mb_size):triples) =
				triples2stmts ??' (acc ++ stmts) triples
				where
				ident = cident2ident cident
				ty = getCDeclType $ CDecl declspecs [(Just cdeclr,mb_init,mb_size)] ni
				zty = ty2zty (ty,[])
				??' = (ident,zty) : ??
				stmts = AST.Decl (VarDeclaration ident (renderpretty ty) zty (ni2loc ni)) (ni2loc ni) : case mb_init of
					Nothing          ??? []
					Just initializer ??? [ Var ident zty (ni2loc ni) ??? initializer2expr ?? zty initializer ]

	expr2ast :: TyEnv ??? Maybe ZType ??? CExpr ??? Expr
	expr2ast ?? mb_target_ty cexpr = case mb_target_ty of
		Nothing        ??? expr
		Just target_ty ??? mb_cast target_ty expr

		where

		lookupIdentTy :: Ident ??? ZType
		lookupIdentTy ident = case lookup ident ?? of
			Nothing ??? error $ "lookupIdentTy " ++ show (Prettyprinter.pretty ident) ++ " not found!"
  			Just ty ??? ty

		loc = ni2loc cexpr

		mb_cast :: ZType ??? Expr ??? Expr
		mb_cast target_ty expr | target_ty /= typeE expr = Cast expr target_ty introLoc
		mb_cast _ expr = expr

		expr = case cexpr of

			CAssign ass_op lexpr rexpr _ ??? Assign lexpr' rexpr' (typeE lexpr') loc
				where
				lexpr' = expr2ast ?? Nothing lexpr
				rexpr' = expr2ast ?? (Just $ typeE lexpr') rexpr
				expr'  = case ass_op of
					CAssignOp ??? rexpr'
					other_op  ??? Binary binop lexpr' rexpr' (typeE lexpr') introLoc where
						Just binop = lookup ass_op [
							(CMulAssOp,Mul),(CDivAssOp,Div),(CRmdAssOp,Rmd),(CAddAssOp,Add),(CSubAssOp,Sub),
							(CShlAssOp,Shl),(CShrAssOp,Shr),(CAndAssOp,BitAnd),(CXorAssOp,BitXOr),(COrAssOp,BitOr) ]

			CCond cond (Just then_expr) else_expr _ ???
				CondExpr (expr2ast ?? (Just cboolTy) cond) (mb_cast max_ty then_expr') (mb_cast max_ty else_expr') max_ty loc
				where
				cond'      = expr2ast ?? (Just cboolTy) cond
				then_expr' = expr2ast ?? Nothing then_expr
				else_expr' = expr2ast ?? Nothing else_expr
				max_ty     = max (typeE then_expr') (typeE else_expr')

			CCast cdecl expr _ ??? expr2ast ?? (Just $ ty2zty (getCDeclType cdecl,[])) expr

			CBinary cbinop expr1 expr2 _ ??? Binary binop (mb_cast arg_ty expr1') (mb_cast arg_ty expr2') res_ty loc
				where
				Just binop = lookup cbinop [
					(CMulOp,Mul),(CDivOp,Div),(CRmdOp,Rmd),(CAddOp,Add),(CSubOp,Sub),(CShlOp,Shl),
					(CShrOp,Shr),(CLeOp,LessEq),(CGrOp,Greater),(CLeqOp,LessEq),(CGeqOp,GreaterEq),(CEqOp,Equals),(CNeqOp,NotEquals),
					(CAndOp,BitAnd),(CXorOp,BitXOr),(COrOp,BitOr),(CLndOp,And),(CLorOp,Or) ]
				expr1' = expr2ast ?? Nothing expr1
				expr2' = expr2ast ?? Nothing expr2
				max_ty = max (typeE expr1') (typeE expr2')
				(res_ty,arg_ty) = case binop of
					op | op `elem` [Mul,Div,Add,Sub,Rmd,Shl,Shr,BitAnd,BitOr,BitXOr] ??? (max_ty,max_ty)
					op | op `elem` [Less,Equals,NotEquals,LessEq,Greater,GreaterEq]  ??? (cboolTy, max_ty)
					op | op `elem` [And,Or]                                          ??? (cboolTy, cboolTy )
					other -> error $ "infer_expr " ++ show other ++ " not implemented"

			CUnary cunop expr _ ??? Unary unop expr' ty loc
				where
				Just unop = lookup cunop [
					(CPreIncOp,PreInc),(CPreDecOp,PreDec),(CPostIncOp,PostInc),(CPostDecOp,PostDec),(CAdrOp,AddrOf),
					(CIndOp,Deref),(CPlusOp,Plus),(CMinOp,Minus),(CCompOp,BitNeg),(CNegOp,Not) ]
				expr'     = expr2ast ?? Nothing expr
				expr'_ty  = typeE expr'
				ty        = case unop of
					AddrOf ??? ZPtr expr'_ty
					Deref  ??? targettyZ expr'_ty
					Not    ??? cboolTy
					op | op `elem` [Plus,Minus,BitNeg,PreInc,PostInc,PreDec,PostDec]
					       ??? expr'_ty

			CIndex arr index _ ??? Index arr' (expr2ast ?? (Just indexTy) index) elem_ty loc
				where
				arr'             = expr2ast ?? Nothing arr
				ZArray elem_ty _ = typeE arr'

			CConst ctconst ??? Constant const (ty2zty (ty,[])) loc
				where
				(const,ty) = case ctconst of
					CIntConst ci@(CInteger _ _ flags) _ ??? ( IntConst (getCInteger ci),         integral $ getIntType flags )
					CCharConst cchar _                  ??? ( CharConst (read $ getCChar cchar), integral TyChar )
					CFloatConst (CFloat f) _            ??? ( FloatConst (read f),               floating $ getFloatType f )
					CStrConst cstring _                 ??? ( StringConst (getCString cstring),  PtrType (integral TyChar) noTypeQuals noAttributes )

			CMember cexpr cident is_ptr ni ??? Member expr member_ident is_ptr member_ty loc
				where
				member_ident = cident2ident cident
				expr         = expr2ast ?? Nothing cexpr
				member_ty    = lookupVarDeclsTy member_ident $ case (is_ptr,typeE expr) of
					(True , ZPtr ZCompound{..}) ??? elemtysZ
					(False,      ZCompound{..}) ??? elemtysZ

				lookupVarDeclsTy :: Ident ??? [VarDeclaration] ??? ZType
				lookupVarDeclsTy ident vardecls = typeVD $ head $ filter ((==ident).identVD) vardecls

			CVar cident ni ??? Var ident (lookupIdentTy ident) loc
				where
				ident = cident2ident cident

			CCall cfun cargs ni ??? Call fun args rettyZ loc
				where
				args     = map (\ (argty,arg) ??? expr2ast ?? (Just argty) arg) (zip argtysZ cargs)
				fun      = expr2ast ?? Nothing cfun
				ZFun{..} = typeE fun

			other ??? error $ "expr2ast " ++ show other ++ " not implemented"
