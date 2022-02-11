{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards #-}

module AST where

import qualified Data.Map.Strict as Map


type AST = TranslUnit ()
type TypedAST = TranslUnit ZType

data Loc = Loc { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int } deriving (Eq)
instance Show Loc where
	show Loc{..} = show fileNameLoc ++ " : line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", length " ++ show lengthLoc

data ZType =
	ZVoid |
	ZInt Int Bool |
	ZFloat Int |
	ZArray ZType (Maybe Integer) |
	ZPtr ZType |
	ZEnum [Ident] |
	ZStruct [VarDecl] |
	ZUnion [VarDecl] deriving (Show)

type TranslUnit a = Map.Map Ident (ExtDecl a)

data VarDecl = VarDecl Ident [Specifier] ZType Loc deriving (Show)

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc } deriving (Show)
instance Eq Ident where
	ident1 == ident2 = nameIdent ident1 == nameIdent ident2 && idIdent ident1 == idIdent ident2

data Specifier = Specifier deriving Show

data ExtDecl a = ExtDecl VarDecl (Either (Maybe (Expr a)) ([VarDecl],Stmt a)) Loc

data Expr a =
	StmtExpr (Stmt a) a Loc |
	Assign (Expr a) (Expr a) a Loc |
	Cast (Expr a) a Loc |
	Call (Expr a) [Expr a] a Loc |
	Unary UnaryOp (Expr a) a Loc |
	Binary BinaryOp (Expr a) (Expr a) a Loc |
	CondExpr (Expr a) (Expr a) (Expr a) a Loc |
	Index (Expr a) (Expr a) a Loc |
	Member (Expr a) Ident Bool a Loc |
	Var Ident a Loc |
	Constant (Const a) a Loc

data UnaryOp = AddrOf | DerefOp | Neg | Exor | Not
data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr |
	Less | Equals | LessEq | Greater | GreaterEq |
	And | Or | BitAnd | BitOr | BitXOr

data Const a =
	IntConst Integer a Loc |
	CharConst Char a Loc |
	FloatConst String a Loc |
	StringConst String a Loc

data Stmt a =
	Label String Loc |
	Compound [Stmt a] Loc |
	IfThenElse (Expr a) (Stmt a) (Stmt a) Loc |
	ExprStmt (Expr a) Loc |
	Loop (Expr a) (Stmt a) Loc |   -- This is a while loop. do-while and for loops are transcribed into this form.
	Return (Maybe (Expr a)) Loc |
	Goto Ident Loc
