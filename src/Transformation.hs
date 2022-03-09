{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,UnicodeSyntax #-}

module Transformation where

--https://hackage.haskell.org/package/uniplate
-- SYB: import Data.Generics

import Control.Monad.State

import AST


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

transformAST :: AST → AST
transformAST ast = forEachExtDecl ast $ id

elimSideEffects :: StateT Int AST → AST
elimSideEffects 