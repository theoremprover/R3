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
transformAST ast = return ast >>=
	elimConstructs >>= elimSideEffects

newIdent :: String → R3 Ident
newIdent prefix = do
	i <- getNewNameCnt 
	return $ Ident (prefix ++ "$" ++ show i) i introLoc

elimConstructs :: TranslUnit → R3 TranslUnit
elimConstructs ast = transformBiM stmt_rules ast
	where

	stmt_rules :: Stmt -> R3 Stmt

	stmt_rules Switch{..} = do
		MachineSpec{..} <- gets machineSpecR3
		val_ident <- newIdent "switch_val"
		let
			val_ty = typeE condS
			val_var = Var val_ident val_ty introLoc
			Compound _ stmts loc = bodyS
		return $ Compound True [
			val_ident ∶∶ val_ty,
			val_var ≔ condS,
  			search_cases (ZInt intSize False) val_var stmts ] loc

  		where

		search_cases _ val_var [] = emptyStmt
		search_cases _ val_var (Default _ : stmts) = Compound False stmts introLoc
		search_cases cboolty val_var (Case {..} : stmts) = 𝗂𝖿 cond cases_filtered_out till_case
			where
			cond                = Binary Equals val_var condS cboolty locS
			isnocase (Case _ _) = False
			isnocase _          = True
			cases_filtered_out  = Compound False (filter isnocase stmts) introLoc
			till_case           = search_cases cboolty val_var (dropWhile isnocase stmts)
		search_cases cboolty val_var (_ : stmts) = search_cases cboolty val_var stmts

	stmt_rules other = return other

elimSideEffects :: TranslUnit → R3 TranslUnit
elimSideEffects ast = transformBiM elim_rules ast
	where
	elim_rules :: Stmt -> R3 Stmt
	elim_rules stmt = case [ stmts |
		Unary op expr ty _ <- universeBi stmt,
		op `elem` [PreInc,PostInc,PreDec,PostDec],
		]
		of
		[] -> return stmt
		stmts -> return $ Compound False stmts introLoc

