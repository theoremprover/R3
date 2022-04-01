{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,UnicodeSyntax,ScopedTypeVariables #-}

module Transformation where


import Control.Monad.Trans.State

-- SYB: import Data.Generics
--https://hackage.haskell.org/package/uniplate
import Data.Generics.Uniplate.Data

import Control.Monad

import AST
import R3Monad
import Prettyprinter
import MachineSpec

{-
	AST transformations:

	rewrite Neq to Not equal
	elimination of expressions with side effects (assignments)
	expand CondExprs to IfThenElse
	rewrite switch/case to if/then/else
	rewrite continue
-}

{-

a = ++x == (y=3) ? 3*(++x) : 2*(y++);

~>

x = x + 1;
y = 3;
if(x==3) { x = x + 1; a = 3*x; }
else { a = 2*y; y = y + 1; }

Sequence Points!

... ( C ? A : B ) ...

~>  

if C then c = A; else c = B;
... c ...


int a = 3;
int* p = &a;
*p = 2;
return a;


int a = 3;
int b = 2;
int* p = &a;
int* q = &b;
int** pp = &p;

-}

transformAST :: TranslUnit → R3 TranslUnit
transformAST ast = do
	machinespec <- gets machineSpecR3
	elimConstructs machinespec ast

newIdent :: String → R3 Ident
newIdent prefix = do
	i <- getNewNameCnt 
	return $ Ident (prefix ++ "$" ++ show i) i introLoc

elimConstructs :: MachineSpec → TranslUnit → R3 TranslUnit
elimConstructs MachineSpec{..} ast = do
	transformBiM rules ast

	where

 	cboolTy :: ZType
	cboolTy = ZInt intSize False

	indexTy :: ZType
	indexTy = ZInt intSize True

 	rules :: Stmt -> R3 Stmt

{-
switch(expr)
{
	case c1 : a;
	          b;
	case c2 : c;
	          break;
	case c3 : d;
	default:  e;
}

~>

switch_val = expr;
if(switch_val==c1)
{
	a;
	b;
	c;
}
else if(switch_val==c2)
{
	c;
}
else if(switch_val==c3)
{
	d;
}
else
{
	e;
}
-}
  	rules Switch{..} = do
  		val_ident <- newIdent "switch_val"
  		let
  			val_ty = typeE condS
  			val_var = Var val_ident val_ty introLoc
  			Compound _ stmts loc = bodyS
  		return $ Compound True [
			val_ident ∶∶ val_ty,
			val_var ≔ condS,
  			search_cases val_var stmts ] loc
  		where
		search_cases val_var [] = emptyStmt
		search_cases val_var (Default _ : stmts) = Compound False stmts introLoc
		search_cases val_var (Case {..} : stmts) = 𝗂𝖿 cond cases_filtered_out till_case where
			cond = Binary Equals val_var condS cboolTy locS
			isnocase (Case _ _) = False
			isnocase _ = True
			cases_filtered_out = Compound False (filter isnocase stmts) introLoc
			till_case = search_cases val_var (dropWhile isnocase stmts)
		search_cases val_var (_ : stmts) = search_cases val_var stmts

	rules other = return other

{-
elimSideEffects :: TranslUnit → R3 TranslUnit
elimSideEffects ast = do
	liftIO $ mapM_ print [ show loc ++ " : " ++ show (pretty stm) |
		Compound _ stmts _ <- universeBi ast,
		stm@(ExprStmt expr loc) <- stmts,
		Unary op _ _ _ :: Expr ZType <- children expr ]
--		op `elem` [PreInc,PostInc,PreDec,PostDec] ]
	return ast
-}

{-
asta = C [ExpStm (V 1), C [ExpStm (I (V 9)),ExpStm (V 3)]]
test = mapM_ print [ (show stm) |
	stm :: Stm <- universeBi asta,
	True ]
-}

{-
infer_expr
	Assign {
		lexprE = Var {identE = Ident {nameIdent = "a", idIdent = 97, locIdent = switchtest.c, line 3, col 9, len 1},
		typeE = Just (DirectType (TyIntegral int) (TypeQuals {constant = False, volatile = False, restrict = False, atomic = False, nullable = False, nonnull = False, clrdonly = False, clwronly = False}) [],[]),
		locE = switchtest.c, line 3, col 9, len 1},
		exprE = Constant {constE = IntConst 0, typeE = Just (DirectType (TyIntegral int) (TypeQuals {constant = False, volatile = False, restrict = False, atomic = False, nullable = False, nonnull = False, clrdonly = False, clwronly = False}) [],[]), locE = switchtest.c, line 3, col 13, len 1},
		typeE = Just (DirectType (TyIntegral int) (TypeQuals {constant = False, volatile = False, restrict = False, atomic = False, nullable = False, nonnull = False, clrdonly = False, clwronly = False}) [],[]), locE = switchtest.c, line 3, col 9, len 1}
		not implemented

-}