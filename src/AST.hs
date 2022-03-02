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

-- The
data ZType =
	ZUnit                                                                                    | -- void is the unit type
	ZBool                                                                                    |
	ZInt       { sizeZ     :: Int,               isunsignedZ   :: Bool                     } |
	ZEnum      { elemsZ    :: [(Ident,Integer)]                                            } |
	ZFloat     { expbitsZ  :: Int,               mantissabitsZ :: Int                      } | -- (mantissabitsZ includes the hidden bit, but excludes sign bit)
	ZArray     { elemtyZ   :: ZType,             arrsizeZ      :: Maybe Integer            } |
	ZPtr       { targettyZ :: ZType                                                        } |
	ZCompound  { comptyZ   :: CompoundType,      elemtysZ      :: [VarDeclaration ZType]   } |
	ZFun       { rettyZ    :: ZType,             isvariadicZ   :: Bool, argtysZ :: [ZType] } |
	ZUnhandled { descrZ    :: String                                                       }
	deriving (Show,Generic,Eq)
instance Ord ZType where
	ZUnit <= _ = True
	_ <= ZUnit = False
	ZBool <= _ = True
	_ <= ZBool = False



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

data FunDef a = FunDef [VarDeclaration a] (Stmt a)
	deriving (Show,Generic)

data Expr a =
	Assign   { lexprE :: Expr a,   exprE   :: Expr a,   typeE  :: a,      locE  :: Loc            } |
	Cast     { exprE  :: Expr a,   typeE   :: a,        locE   :: Loc                             } |
	Call     { exprE  :: Expr a,   argsE   :: [Expr a], typeE  :: a,      locE  :: Loc            } |
	Unary    { unopE  :: UnaryOp,  exprE   :: Expr a,   typeE  :: a,      locE  :: Loc            } |
	Binary   { binopE :: BinaryOp, expr1E  :: Expr a,   expr2E :: Expr a, typeE :: a, locE :: Loc } |
	CondExpr { condE  :: Expr a,   thenE   :: Expr a,   elseE  :: Expr a, typeE :: a, locE :: Loc } |
	Index    { exprE  :: Expr a,   indexE  :: Expr a,   typeE  :: a,      locE  :: Loc            } |
	Member   { exprE  :: Expr a,   memberE :: Ident,    isptrE :: Bool,   typeE :: a, locE :: Loc } |
	Var      { identE :: Ident,    typeE   :: a,        locE   :: Loc                             } |
	Constant { constE :: Const,    typeE   :: a,        locE   :: Loc                             } |
	Comp     { elemsE :: [Expr a], typeE   :: a,        locE   :: Loc                             }
	deriving (Show,Generic)

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
instance (Eq a) => Eq (VarDeclaration a) where
	(VarDeclaration ident1 _ ty1 _) == (VarDeclaration ident2 _ ty2 _) =
		ident1==ident2 && ty1==ty2

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
