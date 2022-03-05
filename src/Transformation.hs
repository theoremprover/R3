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

transformAST :: AST → AST
transformAST ast = forEachExtDecl ast $ id