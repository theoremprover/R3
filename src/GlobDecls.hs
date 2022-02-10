{-# LANGUAGE PackageImports,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module GlobDecls where

import "language-c" Language.C
import Language.C.Data.Ident
import Language.C.Analysis.AstAnalysis
--import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Text.Blaze.Html4.Strict as H
import Text.Blaze.Html.Renderer.String
import Control.Monad
import Data.Map.Strict as Map
import Text.PrettyPrint


globdeclsToHTMLString :: GlobalDecls -> String
globdeclsToHTMLString (GlobalDecls objs tags typedefs) = renderHtml $ docTypeHtml $ do
	H.head $ do
		title "GlobalDecls"
	body $ do
		h1 "GlobalDecls"
		h2 "Defs/Decls"
		showMapTable (render.pretty) objs
		h2 "Tags"
		showMapTable (render.pretty) tags
		h2 "Typedefs"
		showMapTable (\ (TypeDef _ ty _ _) -> (render.pretty) ty) typedefs
	where
	showMapTable showval mapping = table $
		forM_ (assocs mapping) $ \ (key,val) -> tr $ do
			td $ toHtml $ (render.pretty) key
			td $ preEscapedToHtml ("&#x21a6;"::String)
			td $ toHtml $ showval val
