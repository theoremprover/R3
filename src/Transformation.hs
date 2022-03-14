{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,UnicodeSyntax,ScopedTypeVariables,DeriveDataTypeable #-}

module Transformation where


import Control.Monad.Trans.State

-- SYB: import Data.Generics
--https://hackage.haskell.org/package/uniplate
import Data.Generics.Uniplate.Data

import Control.Lens hiding (Const)

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

transformAST :: AST → R3 AST
transformAST ast = elimSideEffects ast

elimSideEffects :: AST → R3 AST
elimSideEffects ast = return ast
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
test = mapM_ print [ (show stm,show exp) |
	stm :: Stm <- universeBi asta,
	I exp <- childrenBi stm ]
