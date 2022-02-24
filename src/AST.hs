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

type ASTMap k v = ASTMap.Map k v

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
	ZCompound CompoundType [VarDeclaration] |
	ZEnum [(Ident,Integer)] |
	ZFun ZType {-isVariadic::-}Bool [ZType] |
	ZUnhandled String
	deriving (Show,Generic)

type TranslUnit = ASTMap Ident ExtDecl

-- AST contains variable declarations, each of them having either
-- 1. maybe an initializer (i.e. a variable declaration), or
-- 2. a statement as body of the defined function
--    (the arguments and their types are in the type of the function identifier)

data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) Stmt) Loc
	deriving (Show,Generic)

data Expr =
	CondExpr Expr Expr Expr ZType Loc |
	StmtExpr Stmt ZType Loc |
	Assign Expr Expr ZType Loc |
	Cast Expr ZType Loc |
	Call Expr [Expr] ZType Loc |
	Unary UnaryOp Expr ZType Loc |
	Binary BinaryOp Expr Expr ZType Loc |
	CondExpr Expr Expr Expr ZType Loc |
	Index Expr Expr ZType Loc |
	Member Expr Ident Bool ZType Loc |
	Var Ident ZType Loc |
	Constant Const ZType Loc |
	Comp [Expr] ZType Loc
	deriving (Show,Generic)

class Typed a where
	typeOf :: a -> ZType
instance Typed Expr where
	typeOf (CondExpr _ _ _ ty _) = ty
	typeOf (StmtExpr _ ty _) = ty
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

data UnaryOp = AddrOf | DerefOp | Neg | Exor | Not
	deriving (Show,Generic)

data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr |
	Less | Equals | LessEq | Greater | GreaterEq |
	And | Or | BitAnd | BitOr | BitXOr
	deriving (Show,Generic)

data VarDeclaration = VarDeclaration {
	identVD      :: Ident,
	sourceTypeVD :: String,
	typeVD       :: ZType,
	locVD        :: Loc }
	deriving (Show,Generic)

data Const =
	IntConst Integer |
	CharConst Char |
	FloatConst String |
	StringConst String
	deriving (Show,Generic)

data Stmt =
	Decls [VarDeclaration] Loc |
	Label Ident Stmt Loc |
	Compound [Stmt] Loc |
	IfThenElse Expr Stmt Stmt Loc |
	ExprStmt Expr Loc |
	While Expr Stmt Loc |
	Return (Maybe Expr) Loc |
	Continue Loc |
	Goto Ident Loc
	deriving (Show,Generic)

{-
	Second step:
	1. expand vardecls into multiple DECLs
	2. expand CondExprs to IfThenElse
	3. dissect expressions with side effects,
	=> Expr no sideeffects

	Output is a list of Computations (Stmt,Expr,Stmt)
-}
