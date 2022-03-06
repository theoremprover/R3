{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE RecordWildCards,UnicodeSyntax #-}

module Transformation where

import AST

{-
	AST transformations:

	rewrite Neq to Not equal
	rewrite expressions containing side effects
	expand CondExprs to IfThenElse
-}

{-

a = ++x == (y=3) ? 3*(++x) : 2*(y++);

~>

x = x + 1;
y = 3;
if(x==3) { x = x + 1; a = 3*x; }
else { a = 2*y; y = y + 1; }

Sequence Points!

-}

transformAST :: AST → AST
transformAST ast = forEachExtDecl ast $ id

