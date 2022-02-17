{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards #-}

module AST where

import qualified Data.Map.Strict as ASTMap

data Loc = Loc { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int } | NoLoc String
	deriving (Eq,Ord)
instance Show Loc where
	show Loc{..} = show fileNameLoc ++ " : line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", length " ++ show lengthLoc
	show (NoLoc s) = "<"++s++">"

data ZType =
	ZUnit |         -- void is the unit type
	ZInt Int Bool | -- ZInt size_bits isUnsigned
	ZFloat Int Int | -- ZFloat exp_bits significand_bits  (significand_bits includes the hidden bit, but excludes sign bit)
	ZArray ZType (Maybe Integer) |
	ZPtr ZType |
	ZEnum [Ident] |
	ZStruct [VarDeclaration] |
	ZUnion [VarDeclaration] |
	ZFun ZType {-isVariadic::-}Bool [ZType] |
	ZUnhandled String
	deriving (Show)

type TranslUnit = ASTMap.Map Ident ExtDecl

data VarDeclaration = VarDeclaration Ident String ZType Loc deriving (Show)

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc } deriving (Show,Ord)
instance Eq Ident where
	ident1 == ident2 = nameIdent ident1 == nameIdent ident2 && idIdent ident1 == idIdent ident2

-- AST contains variable declarations, each of them having either
-- 1. maybe an initializer (i.e. a variable declaration), or
-- 2. a statement as body of the defined function
--    (the arguments and their types are in the type of the function identifier)

data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) Stmt) Loc
	deriving (Show)

data Expr =
	StmtExpr Stmt ZType Loc |
	Assign Expr Expr ZType Loc |
	Cast ZType Expr ZType Loc |
	Call Expr [Expr] ZType Loc |
	Unary UnaryOp Expr ZType Loc |
	Binary BinaryOp Expr Expr ZType Loc |
	CondExpr Expr Expr Expr ZType Loc |
	Index Expr Expr ZType Loc |
	Member Expr Ident Bool ZType Loc |
	Var Ident ZType Loc |
	Constant Const ZType Loc |
{-
	int a[3] = { 1,2,3 };
//	enum X = { ABC=0,DEF=1 };
	y = { .first=1, 2, .sub={ 'a', 3 }, .last=7 };
-}
	Comp [(Maybe Ident,Expr)] ZType Loc
	deriving (Show)

data UnaryOp = AddrOf | DerefOp | Neg | Exor | Not
	deriving (Show)

data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr |
	Less | Equals | LessEq | Greater | GreaterEq |
	And | Or | BitAnd | BitOr | BitXOr
	deriving (Show)

data Const =
	IntConst Integer |
	CharConst Char |
	FloatConst String |
	StringConst String
	deriving (Show)

data Stmt =
	Label String Loc |
	Compound [Stmt] Loc |
	IfThenElse Expr Stmt Stmt Loc |
	ExprStmt Expr Loc |
	Loop Expr Stmt Loc |   -- This is while loop. do-while and for loops are transcribed into this form.
	Return (Maybe Expr) Loc |
	Goto Ident Loc
	deriving (Show)
