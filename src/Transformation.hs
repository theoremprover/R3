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
transformAST ast = return ast --elimConstructs ast >>= elimSideEffects

newIdent :: String -> R3 Ident
newIdent prefix = do
	i <- getNewNameCnt 
	return $ Ident (prefix ++ "$" ++ show i) i introLoc

{-
elimConstructs :: TranslUnit → R3 TranslUnit
elimConstructs ast = transformBiM rules ast
	where

  	rules :: Stmt -> R3 Stmt

  	-- The body compound under While catches break by default,
  	-- and also breaks the "Compound True [body ..." compound, because it is the last compound in the whole compound
	rules (DoWhile cond body loc) = return $ Compound True [body,While cond body loc] loc

	rules (For cond inc body loc) = do
		let body' = Compound True [body,inc] (locS body)
		return $ While cond body' loc

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

	rules (Switch switchval (Compound True bodystmts bodyloc) loc) = do
		newident <- newIdent "switch_val"
		let switchvar = Var newident (typeE switchval) introLoc
		return $ Compound True [
			switchvar ≔ switchval,
			switchbody switchvar bodystmts
			] bodyloc

		where

		switchbody switchvar (Case val loc : stmts) =
			𝗂𝖿 (val ≟ switchvar)
				(Compound False (till_break (body:stmts)) introLoc)
				(switchbody switchvar (skip_till_case stmts))
		switchbody _ (Default stmt _ : stmts) = Compound False (till_break (stmt:stmts)) introLoc

		till_break [] = []
		till_break (Break _ : _) = []
		till_break (stmt : stmts) = stmt : till_break stmts

		skip_till_case [] = []
		skip_till_case stmts@(Case _ _ _ : _) = stmts
		skip_till_case (_ : stmts) = skip_till_case stmts

	rules other = return other

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