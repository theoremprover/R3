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
forEachExtDecl :: AST → (ExtDeclAST → ExtDeclAST) → AST
forEachExtDecl ast f = map f ast
-}

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
{-
	stmt2ast (CWhile cond body True ni) = Compound [body',loop] (ni2loc ni) where
		body' = stmt2ast body
		loop = While (expr2ast cond) body' (ni2loc ni)
	stmt2ast (CFor mb_expr_or_decl (Just cond) mb_inc body ni) = Compound [ini,loop] (ni2loc ni) where
		ini = case mb_expr_or_decl of
			Left (Just ini_expr) → ExprStmt (expr2ast ini_expr) (ni2loc ini_expr)
			Right cdecl          → decl2stmt cdecl
		loop = While (expr2ast cond) body' (ni2loc ni)
		body' = Compound [stmt2ast body,inc] (ni2loc body)
		inc = case mb_inc of
			Nothing       → Compound [] (ni2loc ni)
			Just inc_expr → ExprStmt (expr2ast inc_expr) (ni2loc inc_expr)
-}

transformAST :: AST → R3 AST
transformAST ast = elimConstructs ast >>= elimSideEffects

introLoc = NoLoc "introduced"

newIdent :: String -> R3 Ident
newIdent prefix = do
	i <- getNewNameCnt 
	return $ Ident (prefix ++ "$" ++ show i) i introLoc

elimConstructs :: AST → R3 AST
elimConstructs ast = transformM rule ast where

	rule (DoWhile cond body loc) = return $ Compound [body,While cond body loc] loc

	rule (For ini cond inc body loc) = do
		let body' = Compound [body,inc] (locS body)
		return $ Compound [ini,While cond body' loc] loc

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
	rule (Switch cond body loc) = do
		newident <- newIdent "switch_val"
		let switchvar = Var newident (typeE cond) introLoc
		return $ Compound [
			ExprStmt (Assign switchvar cond (typeE switchvar)) introLoc,
			

elimSideEffects :: AST → R3 AST
elimSideEffects ast = do
	liftIO $ mapM_ print [ show loc ++ " : " ++ show (pretty stm) |
		Compound stmts _ <- universeBi ast,
		stm@(ExprStmt expr loc) <- stmts,
		Unary op _ _ _ :: Expr ZType <- children expr ]
--		op `elem` [PreInc,PostInc,PreDec,PostDec] ]
	return ast
{-
elimSideEffects :: AST → R3 AST
elimSideEffects ast = do
	liftIO $ mapM_ putStrLn [ show (pretty stmt) |
		stmt :: Stmt ZType <- universeBi ast,
		Unary op expr _ _ :: Expr ZType <- childrenBi stmt,
		op `elem` [PreInc,PostInc,PreDec,PostDec]
		]
	return ast
-}

asta = C [ExpStm (V 1), C [ExpStm (I (V 9)),ExpStm (V 3)]]
test = mapM_ print [ (show stm) |
	stm :: Stm <- universeBi asta,
	True ]
