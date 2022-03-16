{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DeriveDataTypeable,RecordWildCards,DeriveGeneric,TypeSynonymInstances,FlexibleInstances,FlexibleContexts,UnicodeSyntax #-}

module AST where

import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.String

import Data.Data
import Data.Generics.Uniplate.Data

{-
data Te = Te [Te] | TeInt Int deriving (Show,Generic)
testTe = Te [ Te [TeInt 1,TeInt 2], Te [ TeInt 3, TeInt 4 ]]

testG = [[1,2,3],[4,5,6]] :: [[Int]]
-}
data Stm = ExpStm Exp | C [Stm]
	deriving (Show,Generic,Data,Typeable)
data Exp = V Int | I Exp
	deriving (Show,Generic,Data,Typeable)


type AST               = TranslUnit ZType
type ExtDeclAST        = ExtDecl ZType
type ExprAST           = Expr ZType
type StmtAST           = Stmt ZType
type VarDeclarationAST = VarDeclaration ZType
type FunDefAST         = FunDef ZType

data Loc =
	NoLoc { nolocDescrLoc :: String } |
	Loc   { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int }
	deriving (Eq,Ord,Generic,Data,Typeable)
instance Show Loc where
	show Loc{..}   = fileNameLoc ++ ", line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", len " ++ show lengthLoc
	show (NoLoc s) = s
instance Pretty Loc where
	pretty noloc@NoLoc{..} = viaShow noloc
	pretty Loc{..}         = pretty $ "line " ++ show lineLoc

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc } deriving (Show,Ord,Generic,Data,Typeable)
instance Eq Ident where
	ident1 == ident2 = nameIdent ident1 == nameIdent ident2 && idIdent ident1 == idIdent ident2
instance Pretty Ident where
	pretty ident = pretty $ nameIdent ident

data VarDeclaration a = VarDeclaration {
	identVD      :: Ident,
	sourceTypeVD :: String,
	typeVD       :: a,
	locVD        :: Loc }
	deriving (Show,Generic,Ord,Data,Typeable)
instance (Eq a) => Eq (VarDeclaration a) where
	(VarDeclaration ident1 _ ty1 _) == (VarDeclaration ident2 _ ty2 _) = ident1==ident2 && ty1==ty2
instance (Pretty a) => Pretty (VarDeclaration a) where
	pretty (VarDeclaration ident _ ty _) = pretty ty <+> pretty ident

data CompoundType = Struct | Union deriving (Show,Eq,Ord,Generic,Data,Typeable)
instance Pretty CompoundType where
	pretty Struct = pretty "struct"
	pretty Union  = pretty "union"

-- The ordering of the constructors have to reflect the C casting hierarchy
data ZType =
	ZUnit | -- void is the unit type
	ZBool |
	ZInt       { sizeZ     :: Int,    isunsignedZ   :: Bool } |
	ZEnum      { nameZ     :: String, elemsZ        :: [(Ident,Integer)] } |
	ZFloat     { expbitsZ  :: Int,    mantissabitsZ :: Int } | -- (mantissabitsZ includes the hidden bit, but excludes sign bit)
	ZArray     { elemtyZ   :: ZType,  arrsizeZ      :: Maybe Integer } |
	ZPtr       { targettyZ :: ZType } |
	ZCompound  { nameZ     :: String, comptyZ       :: CompoundType, elemtysZ :: [VarDeclaration ZType] } |
	ZFun       { rettyZ    :: ZType,  isvariadicZ   :: Bool, argtysZ          :: [ZType] } |
	ZUnhandled { descrZ    :: String }
	deriving (Show,Generic,Eq,Ord,Data,Typeable)
instance Pretty ZType where
	pretty ZUnit          = pretty "void"
	pretty ZBool          = pretty "bool"
	pretty ZInt{..}       = pretty (if isunsignedZ then "u" else "") <> pretty "int" <> viaShow sizeZ
	pretty ZEnum{..}      = pretty "enum" <+> pretty nameZ
	pretty ZFloat{..}     = pretty "float" <> viaShow (mantissabitsZ+expbitsZ)
	pretty ZArray{..}     = pretty elemtyZ <> brackets emptyDoc
	pretty ZPtr{..}       = pretty targettyZ <> pretty "*"
	pretty ZCompound{..}  = pretty comptyZ <+> pretty nameZ
	pretty ZFun{..}       = (if isvariadicZ then pretty "variadic" <+> emptyDoc else emptyDoc) <> pretty rettyZ <+>
                                parens (hcat $ punctuate comma $ map pretty argtysZ)
	pretty ZUnhandled{..} = pretty "UNHANDLED" <+> pretty descrZ


prettyTranslUnitString :: (Pretty a) => TranslUnit a → String
prettyTranslUnitString translunit = renderString $ layoutPretty (LayoutOptions $ AvailablePerLine 120 0.4) (pretty translunit)

type TranslUnit a = [ExtDecl a]
instance {-# OVERLAPS #-} (Pretty a) => Pretty (TranslUnit a) where
	pretty translunit = vcat $ map pretty translunit

lookupExtDef :: String → TranslUnit a → ExtDecl a
lookupExtDef name translunit = head $ filter ((==name).nameIdent.identVD.varDeclED) translunit

data ExtDecl a = ExtDecl {
	varDeclED :: VarDeclaration a,
	-- AST contains variable declarations, each of them having either
	-- 1. maybe an initializer (i.e. a variable declaration), or
	-- 2. a statement as body of the defined function
	--    (the arguments and their types are in the type of the function identifier)
	bodyED    :: Either (Maybe (Expr a)) (FunDef a),
	locED     :: Loc }
	deriving (Show,Generic,Data,Typeable)
instance (Pretty a) => Pretty (ExtDecl a) where
	pretty (ExtDecl vardecl@VarDeclaration{..} body loc) = commentExtDecl vardecl $ case body of
		Left Nothing     → pretty vardecl <> semi <+> locComment loc
		Left (Just expr) → pretty vardecl <+> equals <+> pretty expr <> semi  <+> locComment loc
		Right fundef     → pretty vardecl <+> pretty fundef  <+> locComment loc

commentExtDecl vardecl doc = vcat [hardline,pretty comment,emptyDoc,doc]
	where
	p1 = "// ==== "
	p2 = " " ++ repeat '='
	name = nameIdent $ identVD vardecl
	comment = p1 ++ name ++ take (120 - (length p1 + length name)) p2

data FunDef a = FunDef [VarDeclaration a] (Stmt a)
	deriving (Show,Generic,Data,Typeable)
instance (Pretty a) => Pretty (FunDef a) where
	pretty (FunDef argdecls body) = vcat [ parens (hsep $ punctuate comma $ map pretty argdecls), pretty body ]

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
	Constant { _constE :: Const,    typeE   :: a,        locE   :: Loc                             } |
	Comp     { elemsE :: [Expr a], typeE   :: a,        locE   :: Loc                             }
	deriving (Show,Generic,Data,Typeable)
instance (Pretty a) => Pretty (Expr a) where
	pretty (Assign lexpr expr ty _)          = pretty lexpr <+> colon <> colon <+> pretty ty <+> equals <+> pretty expr
	pretty (Cast expr ty _)                  = parens $ parens (pretty ty) <> pretty expr
	pretty (Call fun args _ _)               = pretty fun <> parens (hsep $ punctuate comma $ map pretty args)
	pretty (Unary op expr _ _) | op `elem` [PostInc,PostDec] = pretty expr <> pretty op
	pretty (Unary op expr _ _)               = pretty op <> pretty expr 
	pretty (Binary op expr1 expr2 _ _)       = parens $ pretty expr1 <+> pretty op <+> pretty expr2
	pretty (CondExpr cond then_e else_e _ _) = parens $ pretty cond <+> pretty "?" <+> pretty then_e <+> colon <+> pretty else_e
	pretty (Index expr ix _ _)               = pretty expr <> brackets (pretty ix)
	pretty (Member expr member isptr _ _)    = pretty expr <> (if isptr then pretty "->" else dot) <> pretty member
	pretty (Var ident _ _)                   = pretty ident
	pretty (Constant con _ _)                = pretty con
	pretty (Comp elems _ _)                  = braces $ hsep $ punctuate comma $ map pretty elems

data UnaryOp = AddrOf | Deref | Plus | Minus | BitNeg | Not | PreInc | PostInc | PreDec | PostDec
	deriving (Show,Generic,Eq,Data,Typeable)
instance Pretty UnaryOp where
	pretty AddrOf  = pretty "&"
	pretty Deref   = pretty "*"
	pretty Plus    = pretty "+"
	pretty Minus   = pretty "-"
	pretty BitNeg  = pretty "~"
	pretty Not     = pretty "!"
	pretty PreInc  = pretty "++"
	pretty PostInc = pretty "++"
	pretty PreDec  = pretty "--"
	pretty PostDec = pretty "--"

data BinaryOp =
	Mul | Div | Add | Sub | Rmd | Shl | Shr | BitAnd | BitOr | BitXOr |
	Less | Equals | NotEquals | LessEq | Greater | GreaterEq | And | Or	
	deriving (Show,Generic,Eq,Data,Typeable)
instance Pretty BinaryOp where
	pretty Mul       = pretty "&"
	pretty Div       = pretty "*"
	pretty Add       = pretty "+"
	pretty Sub       = pretty "-"
	pretty Rmd       = pretty "%"
	pretty Shl       = pretty "<<"
	pretty Shr       = pretty ">>"
	pretty BitAnd    = pretty "&"
	pretty BitOr     = pretty "|"
	pretty BitXOr    = pretty "^"
	pretty Less      = pretty "<"
	pretty Equals    = pretty "=="
	pretty NotEquals = pretty "!="
	pretty LessEq    = pretty "<="
	pretty Greater   = pretty ">"
	pretty GreaterEq = pretty ">="
	pretty And       = pretty "&&"
	pretty Or        = pretty "||"

data Const =
	IntConst Integer |
	CharConst Char |
	FloatConst String |
	StringConst String
	deriving (Show,Generic,Data,Typeable)
instance Pretty Const where
	pretty (IntConst i)    = pretty i
	pretty (CharConst c)   = pretty c
	pretty (FloatConst s)  = pretty s
	pretty (StringConst s) = viaShow s

data Stmt a =
	Decls      { vardeclsS :: [VarDeclaration a], locS      :: Loc } |
	Label      { identS    :: Ident,              stmtS     :: Stmt a, locS      :: Loc } |
	Compound   { stmtsS    :: [Stmt a],           locS      :: Loc } |
	IfThenElse { condS     :: Expr a,             thenstmtS :: Stmt a, elsestmtS :: Stmt a, locS :: Loc } |
	ExprStmt   { exprS     :: Expr a,             locS      :: Loc } |
	While      { condS     :: Expr a,             bodyS     :: Stmt a, locS      :: Loc } |
	DoWhile    { condS     :: Expr a,             bodyS     :: Stmt a, locS      :: Loc } |
	For        { initS     :: Expr a,             condS     :: Expr a, incS :: Stmt a,      bodyS     :: Stmt a, locS      :: Loc } |
	Switch     { condS     :: Expr a,             bodyS     :: Stmt a, locS      :: Loc } |
	Case       { condS     :: Expr a,             bodyS     :: Stmt a, locS      :: Loc } |
	Cases      { loCondS   :: Expr a,             hiCondS   :: Expr a, bodyS     :: Stmt a, locS      :: Loc } |
	Return     { mbexprS   :: Maybe (Expr a),     locS      :: Loc } |
	Continue   { locS      :: Loc } |
	Default    { bodyS     :: Stmt a,             locS      :: Loc } |
	Break      { locS      :: Loc } |
	Goto       { identS    :: Ident,              locS      :: Loc }
	deriving (Show,Generic,Data,Typeable)
instance (Pretty a) => Pretty (Stmt a) where
	pretty (Compound stmts _) = vcat [ nest 4 $ vcat $ lbrace : map pretty stmts, rbrace ]
	pretty (IfThenElse cond then_s else_s loc) = vcat $ [ pretty "if" <> parens (pretty cond) <+> locComment loc, pretty then_s ] ++ case else_s of
		Compound [] _ → []
		else_stmt     → [ mb_singleline else_stmt $ vcat [ pretty "else", pretty else_stmt ] ]
		where
		mb_singleline :: (Stmt a) -> Doc ann -> Doc ann
		mb_singleline comp doc = case comp of
			Compound _ _ → doc
			_            → nest 4 doc
	pretty (While cond body loc) = vcat [ pretty "while" <> parens (pretty cond) <+> locComment loc, pretty body ]
	pretty stmt = hcat [ stmt_doc, locComment (locS stmt) ] where
		stmt_doc = case stmt of
			Decls vardecls _                → vcat $ punctuate semi $ map pretty vardecls
			Label ident stmt _              → vcat [ pretty ident <> colon, pretty stmt ]
			ExprStmt expr _                 → pretty expr <> semi
			Return mb_expr _                → pretty "return" <> parens (maybe emptyDoc pretty mb_expr) <> semi
			Continue _                      → pretty "continue" <> semi
			Break _                         → pretty "break" <> semi
			Goto ident _                    → pretty "goto" <+> pretty ident <> semi

locComment loc = column $ \ col → (pretty $ take (120 - col) (repeat ' ') ++ "// ----------") <+> pretty loc

