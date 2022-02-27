{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,DeriveGeneric #-}

module AST where

import GHC.Generics
import qualified Data.Map.Strict as ASTMap

{-
data Te = Te [Te] | TeInt Int deriving (Show,Generic)
testTe = Te [ Te [TeInt 1,TeInt 2], Te [ TeInt 3, TeInt 4 ]]

testG = [[1,2,3],[4,5,6]] :: [[Int]]
-}

type ASTMap v = ASTMap.Map Ident v

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc } deriving (Show,Ord,Generic)
instance Eq Ident where
	ident1 == ident2 = nameIdent ident1 == nameIdent ident2 && idIdent ident1 == idIdent ident2

data Loc =
	Loc { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int } |
	NoLoc String
	deriving (Eq,Ord,Generic)
instance Show Loc where
	show Loc{..} = show fileNameLoc ++ " : line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", length " ++ show lengthLoc
	show (NoLoc s) = s

data CompoundType = Struct | Union deriving (Show,Eq,Ord,Generic)

data ZType =
	ZUnit |          -- void is the unit type
	ZInt Int Bool |  -- ZInt size_bits isUnsigned
	ZFloat Int Int | -- ZFloat exp_bits significand_bits  (significand_bits includes the hidden bit, but excludes sign bit)
	ZArray ZType (Maybe Integer) |
	ZPtr ZType |
	ZCompound CompoundType [VarDeclaration ZType] |
	ZEnum [(Ident,Integer)] |
	ZFun ZType {-isVariadic::-}Bool [ZType] |
	ZUnhandled String
	deriving (Show,Generic)

type TranslUnit a = ASTMap (ExtDecl a)

-- AST contains variable declarations, each of them having either
-- 1. maybe an initializer (i.e. a variable declaration), or
-- 2. a statement as body of the defined function
--    (the arguments and their types are in the type of the function identifier)

data ExtDecl a = ExtDecl {
	varDeclED :: VarDeclaration a,
	bodyED    :: Either (Maybe (Expr a)) (FunDef a),
	locED     :: Loc }
	deriving (Show,Generic)

data FunDef a = FunDef [VecDeclaration a] (Stmt a)
	deriving (Show,Generic)

data Expr a =
	Assign (Expr a) (Expr a) a Loc |
	Cast (Expr a) a Loc |
	Call (Expr a) [Expr a] a Loc |
	Unary UnaryOp (Expr a) a Loc |
	Binary BinaryOp (Expr a) (Expr a) a Loc |
	CondExpr (Expr a) (Expr a) (Expr a) a Loc |
	Index (Expr a) (Expr a) a Loc |
	Member (Expr a) Ident Bool a Loc |
	Var Ident a Loc |
	Constant Const a Loc |
	Comp [Expr a] a Loc
	deriving (Show,Generic)

typeOf :: Expr ty -> ty
typeOf (Assign _ _ ty _) = ty
typeOf (Cast _ ty _) = ty
typeOf (Call _ _ ty _) = ty
typeOf (Unary _ _ ty _) = ty
typeOf (Binary _ _ _ ty _) = ty
typeOf (CondExpr _ _ _ ty _) = ty
typeOf (Index _ _ ty _) = ty
typeOf (Member _ _ _ ty _) = ty
typeOf (Var _ ty _) = ty
typeOf (Constant _ ty _) = ty
typeOf (Comp _ ty _) = ty

data UnaryOp = AddrOf | Deref | Plus | Minus | ExOr | Not |
	PreInc | PostInc | PreDec | PostDec
	deriving (Show,Generic)

data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr |
	Less | Equals | NotEquals | LessEq | Greater | GreaterEq |
	And | Or | BitAnd | BitOr | BitXOr
	deriving (Show,Generic)

data VarDeclaration a = VarDeclaration {
	identVD      :: Ident,
	sourceTypeVD :: String,
	typeVD       :: a,
	locVD        :: Loc }
	deriving (Show,Generic)

data Const =
	IntConst Integer |
	CharConst Char |
	FloatConst String |
	StringConst String
	deriving (Show,Generic)

data Stmt a =
	Decls [VarDeclaration a] Loc |
	Decl (VarDeclaration a) Loc |
	Label Ident (Stmt a) Loc |
	Compound [Stmt a] Loc |
	IfThenElse (Expr a) (Stmt a) (Stmt a) Loc |
	ExprStmt (Expr a) Loc |
	While (Expr a) (Stmt a) Loc |
	Return (Maybe (Expr a)) Loc |
	Continue Loc |
	Break Loc |
	Goto Ident Loc
	deriving (Show,Generic)

{-
	AST transformations:

	expand vardecls into multiple DECLs
	rewrite loops to While
	rewrite Neq to Not equal
	rewrite expressions containing side effects
	expand CondExprs to IfThenElse
	type annotation
-}
