{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards #-}

module AST where

type AST = TranslUnit ()
type TypedAST = TranslUnit CType

data Loc = Loc { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int } deriving (Eq)
instance Show Loc where
	show Loc{..} = show fileNameLoc ++ " : line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", length " ++ show lengthLoc

data CType =
	CVoid |
	CInt Int Bool |
	CFloat Int |
	CArray CType (Maybe Integer)
	CPtr CType |
	CEnum [Ident] |
	CStruct [VarDecl] |
	CUnion [VarDecl] deriving (Show,Eq)

data TranslUnit a = TranslUnit [ExtDecl a] Location

data VarDecl = VarDecl Ident [Specifier] CType Loc

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc }

data Specifier = ()

data ExtDecl a = ExtDecl VarDecl (Either (Maybe (Expr a)) ([VarDecl],Stmt a)) Loc

data Expr a =
	StmtExpr Stmt a Loc |
	Assign (Expr a) (Expr a) a Loc |
	Cast (Expr a) a Loc |
	Call (Expr a) [Expr a] a Loc |
	Unary UnaryOp (Expr a) a Loc |
	Binary BinaryOp (Expr a) (Expr a) a Loc |
	CondExpr (Expr a) (Expr a) (Expr a) a Loc |
	Index (Expr a) (Expr a) a Loc |
	Member LExpr Ident Bool a Loc |
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
	Compound [Statement] Loc |
	IfThenElse (Expr a) Stmt Stmt Loc |
	ExprStmt (Expr a) Loc |
	Loop (Expr a) Stmt Loc |   -- This is a while loop. do-while and for loops are transcribed into this form.
	Return (Maybe (Expr a)) Loc |
	Goto Ident Loc
