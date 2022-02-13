{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards #-}

module AST where

import qualified Data.Map.Strict as ASTMap

data Loc = Loc { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int } deriving (Eq,Ord)
instance Show Loc where
	show Loc{..} = show fileNameLoc ++ " : line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", length " ++ show lengthLoc

data ZType =
	ZVoid |
	ZInt Int Bool |
	ZFloat Int |
	ZArray ZType (Maybe Int) |
	ZPtr ZType |
	ZEnum [Ident] |
	ZStruct [VarDeclaration] |
	ZUnion [VarDeclaration] deriving (Show)

type TranslUnit = ASTMap.Map Ident ExtDecl

data VarDeclaration = VarDeclaration Ident ZType Loc deriving (Show)

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc } deriving (Show,Ord)
instance Eq Ident where
	ident1 == ident2 = nameIdent ident1 == nameIdent ident2 && idIdent ident1 == idIdent ident2

-- AST is a list of declarations, each of them being either
-- 1. a variable declaration (maybe with an initializer), or
-- 2. a function declaration together with a definition of the function
data ExtDecl = ExtDecl VarDeclaration (Either (Maybe Expr) ([VarDeclaration],Stmt)) Loc

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
	Constant Const ZType Loc

data UnaryOp = AddrOf | DerefOp | Neg | Exor | Not
data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr |
	Less | Equals | LessEq | Greater | GreaterEq |
	And | Or | BitAnd | BitOr | BitXOr

data Const =
	IntConst Integer |
	CharConst Char |
	FloatConst String |
	StringConst String

data Stmt =
	Label String Loc |
	Compound [Stmt] Loc |
	IfThenElse Expr Stmt Stmt Loc |
	ExprStmt Expr Loc |
	Loop Expr Stmt Loc |   -- This is while loop. do-while and for loops are transcribed into this form.
	Return (Maybe Expr) Loc |
	Goto Ident Loc
