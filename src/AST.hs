{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DeriveDataTypeable,RecordWildCards,DeriveGeneric,TypeSynonymInstances,FlexibleInstances,FlexibleContexts,UnicodeSyntax #-}

module AST where

import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.String

import Data.Data
import Data.Generics.Uniplate.Data


type TyEnvItem = (Ident,ZType)
instance {-# OVERLAPS #-} Pretty TyEnvItem where
	pretty (ident,zty) = pretty ident <+> colon <> colon <+> pretty zty
type TyEnv = [TyEnvItem]
instance {-# OVERLAPS #-} Pretty TyEnv where
	pretty tyenv = vcat $ map pretty tyenv

data Loc =
	NoLoc { nolocDescrLoc :: String } |
	Loc   { fileNameLoc::String, lineLoc::Int, columnLoc::Int, lengthLoc::Int }
	deriving (Eq,Ord,Generic,Data,Typeable)
instance Show Loc where
	show Loc{..}   = fileNameLoc ++ ", line " ++ show lineLoc ++ ", col " ++ show columnLoc ++ ", len " ++ show lengthLoc
	show (NoLoc s) = s
instance Pretty Loc where
	pretty noloc@NoLoc{..} = viaShow noloc
	pretty Loc{..}         = pretty $ fileNameLoc ++ ": line " ++ show lineLoc

introLoc = NoLoc "introduced"

data Ident = Ident { nameIdent::String, idIdent::Int, locIdent::Loc } deriving (Show,Ord,Generic,Data,Typeable)
instance Eq Ident where
	ident1 == ident2 = nameIdent ident1 == nameIdent ident2 && idIdent ident1 == idIdent ident2
instance Pretty Ident where
	pretty ident = pretty $ nameIdent ident

data VarDeclaration = VarDeclaration {
	identVD      :: Ident,
	sourceTypeVD :: String,
	typeVD       :: ZType,
	locVD        :: Loc }
	deriving (Show,Generic,Ord,Data,Typeable)
instance Eq VarDeclaration where
	(VarDeclaration ident1 _ ty1 _) == (VarDeclaration ident2 _ ty2 _) = ident1==ident2 && ty1==ty2
instance Pretty VarDeclaration where
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
	ZCompound  { nameZ     :: String, comptyZ       :: CompoundType, elemtysZ :: [VarDeclaration] } |
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


prettyTranslUnitString :: TranslUnit → String
prettyTranslUnitString translunit = renderString $ layoutPretty (LayoutOptions $ AvailablePerLine 120 0.4) (pretty translunit)

type TranslUnit = [ExtDecl]
instance {-# OVERLAPS #-} Pretty TranslUnit where
	pretty translunit = vcat $ map pretty translunit

lookupExtDef :: String → TranslUnit → ExtDecl
lookupExtDef name translunit = head $ filter ((==name).nameIdent.identVD.varDeclED) translunit

data ExtDecl = ExtDecl {
	varDeclED :: VarDeclaration,
	bodyED    :: Either (Maybe Expr) FunDef,
	locED     :: Loc }
	deriving (Show,Generic,Data,Typeable)
instance Pretty ExtDecl where
	pretty (ExtDecl vardecl@VarDeclaration{..} body loc) = commentExtDecl vardecl loc $ case body of
		Left Nothing     → pretty vardecl <> semi <+> locComment loc
		Left (Just expr) → pretty vardecl <+> equals <+> pretty expr <> semi <+> locComment loc
		Right fundef     → pretty vardecl <+> pretty fundef

commentExtDecl vardecl loc doc = vcat [hardline,comment,emptyDoc,doc]
	where
	p1 = "// ==== "
	p2 = " " ++ repeat '='
	name = nameIdent (identVD vardecl)
	comment = pretty p1 <+> pretty name <+> parens (pretty loc) <+> pretty (take (120 - (length p1 + length name)) p2)

data FunDef = FunDef [VarDeclaration] Stmt
	deriving (Show,Generic,Data,Typeable)
instance Pretty FunDef where
	pretty (FunDef argdecls body) = vcat [ parens (hsep $ punctuate comma $ map pretty argdecls), pretty body ]

data Expr =
	Assign   { lexprE :: Expr,   exprE   :: Expr,   typeE  :: ZType,      locE  :: Loc            } |
	Cast     { exprE  :: Expr,   typeE   :: ZType,  locE   :: Loc                             } |
	Call     { exprE  :: Expr,   argsE   :: [Expr], typeE  :: ZType,      locE  :: Loc            } |
	Unary    { unopE  :: UnaryOp,  exprE   :: Expr,   typeE  :: ZType,      locE  :: Loc            } |
	Binary   { binopE :: BinaryOp, expr1E  :: Expr,   expr2E :: Expr, typeE :: ZType, locE :: Loc } |
	CondExpr { condE  :: Expr,   thenE   :: Expr,   elseE  :: Expr, typeE :: ZType, locE :: Loc } |
	Index    { exprE  :: Expr,   indexE  :: Expr,   typeE  :: ZType,      locE  :: Loc            } |
	Member   { exprE  :: Expr,   memberE :: Ident,    isptrE :: Bool,   typeE :: ZType, locE :: Loc } |
	Var      { identE :: Ident,    typeE   :: ZType,        locE   :: Loc                             } |
	Constant { constE :: Const,    typeE   :: ZType,        locE   :: Loc                             } |
	Comp     { elemsE :: [Expr], typeE   :: ZType,        locE   :: Loc                             }
	deriving (Show,Generic,Data,Typeable)
instance Pretty Expr where
	pretty (Assign lexpr expr ty _)          = pretty lexpr <+> equals <+> pretty expr
	pretty (Cast expr ty _)                  = parens $ parens (pretty ty) <> pretty expr
	pretty (Call fun args _ _)               = pretty fun <> parens (hsep $ punctuate comma $ map pretty args)
	pretty (Unary op expr _ _) | op `elem` [PostInc,PostDec]
		                                     = pretty expr <> pretty op
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

data Stmt =
	Decl       { vardeclS  :: VarDeclaration,   locS      :: Loc } |
	Label      { identS    :: Ident,            locS      :: Loc } |
	Compound   { catchBrkS :: Bool,             stmtsS    :: [Stmt], locS      :: Loc } |
	IfThenElse { condS     :: Expr,             thenstmtS :: Stmt,   elsestmtS :: Stmt, locS :: Loc } |
	ExprStmt   { exprS     :: Expr,             locS      :: Loc } |
	While      { isDoWhile :: Bool,             condS     :: Expr,             bodyS     :: Stmt,   locS      :: Loc } |
	Switch     { condS     :: Expr,             bodyS     :: Stmt,   locS      :: Loc } |
	Case       { condS     :: Expr,             locS      :: Loc } |
	Cases      { loCondS   :: Expr,             hiCondS   :: Expr,   locS      :: Loc } |
	Default    { locS      :: Loc } |
	Return     { mbexprS   :: Maybe (Expr),     locS      :: Loc } |
	Continue   { locS      :: Loc } |
	Break      { locS      :: Loc } |
	Goto       { identS    :: Ident,            locS      :: Loc }
	deriving (Show,Generic,Data,Typeable)
instance Pretty Stmt where
	pretty (Compound breaks stmts _) = vcat [ nest 4 $ vcat $
		((lbrace <+> pretty (if breaks then "         // catches break" else "")) : map pretty stmts), rbrace ]
	pretty (IfThenElse cond then_s else_s loc) = vcat $ [ pretty "if" <> parens (pretty cond) <+> locComment loc, pretty then_s ] ++ case else_s of
		Compound _ [] _ → []
		else_stmt       → [ vcat [ pretty "else", pretty else_stmt ] ]
		where
		mb_singleline :: Stmt -> Doc ann -> Doc ann
		mb_singleline comp doc = case comp of
			Compound _ _ _ → doc
			_              → nest 4 doc
	pretty (While False cond body loc) = vcat [ pretty "while" <> parens (pretty cond) <+> locComment loc, pretty body ]
	pretty (While True cond body loc) = vcat [ pretty "do" <+> locComment loc, pretty body, pretty "while" <> parens (pretty cond) ]
	pretty (Switch val body loc) = vcat [ pretty "switch" <> parens (pretty val) <+> locComment loc, pretty body ]
	pretty (Case cond loc) = pretty "case" <+> pretty cond <+> pretty ":" <+> locComment loc
	pretty (Default loc) = pretty "default" <> pretty ":" <+> locComment loc
	pretty (Cases locond hicond loc) = pretty "case" <+> pretty locond <+> pretty "..." <+> pretty hicond <+> pretty ":" <+> locComment loc
	pretty stmt = stmt_doc <+> locComment (locS stmt) where
		stmt_doc = case stmt of
			Decl vardecl _     → pretty vardecl <> semi
			Label ident _      → pretty ident <> colon
			ExprStmt expr _    → pretty expr <> semi
			Return mb_expr _   → pretty "return" <> parens (maybe emptyDoc pretty mb_expr) <> semi
			Continue _         → pretty "continue" <> semi
			Break _            → pretty "break" <> semi
			Goto ident _       → pretty "goto" <+> pretty ident <> semi
			other              → error $ "pretty " ++ show other ++ " not implemented"

locComment loc = column $ \ col → (pretty $ take (120 - col) (repeat ' ') ++ "// ----------") <+> pretty loc

emptyStmt :: Stmt
emptyStmt = Compound False [] introLoc


------ 𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓

infixr 2 ≔
(≔) :: Expr -> Expr -> Stmt
a ≔ b = ExprStmt (Assign a b (typeE a) (locE a)) (locE a)

𝗂𝖿 :: Expr -> Stmt -> Stmt -> Stmt
𝗂𝖿 cond then_stmt else_stmt = IfThenElse cond then_stmt else_stmt introLoc

infixr 2 ∶∶
(∶∶) :: Ident -> ZType -> Stmt
ident ∶∶ ty = Decl (VarDeclaration ident "<none>" ty introLoc) introLoc