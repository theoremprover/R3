{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards #-}

module AST where

data Loc = Loc { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int } deriving (Eq)
instance Show Loc where
	show Loc{..} = show fileNameLoc ++ " : line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", length " ++ show lengthLoc

data CType =
	CVoid |
	CInt Int Bool |
	CFloat Int |
	CArray CType (Maybe Int)
	CPtr CType |
	CEnum [Ident] |
	CStruct [VarDecl] |
	CUnion [VarDecl]

data TranslUnit = TranslUnit [ExtDecl] Location

data VarDecl = VarDecl Ident CType [Specifier] Loc

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc }

data Specifier = ()

data ExtDecl = ExtDecl VarDecl (Either (Maybe Expr) ([VarDecl],Stmt)) Loc

data Expr =
	StmtExpr Stmt Loc |
	Assign LExpr Expr CType Loc |
	Cast Expr CType Loc |
	Call Expr [Expr] CType Loc |
	Unary UnaryOp Expr CType Loc |
	Binary BinaryOp Expr Expr CType Loc |
	CondExpr Expr Expr Expr CType Loc |
	Index Expr Expr CType Loc |
	Member LExpr Ident Bool CType Loc |
	Var Ident CType Loc |
	Constant Const Loc

data UnaryOp = AddrOf | DerefOp | Neg | Exor | Not
data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr |
	Less | Equals | LessEq | Greater | GreaterEq |
	And | Or | BitAnd | BitOr | BitXOr

data Const =
	IntConst Integer CType Loc |
	CharConst Char CType Loc |
	FloatConst String CType Loc |
	StringConst String CType Loc

data Stmt =
	Label String Loc |
	Compound [Statement] Loc |
	IfThenElse Expr Stmt Stmt Loc |
	ExprStmt Expr Loc |
	Loop Expr Stmt Loc |   -- This is a while loop. do-while and for loops are transcribed into this form.
	Return (Maybe Expr) Loc |
	Goto Ident Loc
