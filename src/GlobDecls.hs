{-# LANGUAGE PackageImports,OverloadedStrings,RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module GlobDecls where

import Language.C.Pretty
import Language.C.Analysis
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import Text.Blaze.Html4.Strict as H
import Text.Blaze.Html.Renderer.String
import Control.Monad
import Data.Map.Strict as Map
import Text.PrettyPrint

--import Prettyprinter
--import Prettyprinter.Render.String

import AST

showMapTable :: (k -> String) -> (v -> String) -> Map.Map k v -> Html
showMapTable show_k show_v mapping = table $
	forM_ (assocs mapping) $ \ (key,val) -> tr $ do
		td $ toHtml $ show_k key
		td $ preEscapedToHtml ("&#x21a6;"::String)
		td $ toHtml $ show_v val

globdeclsToHTMLString :: GlobalDecls -> String
globdeclsToHTMLString (GlobalDecls objs tags typedefs) = renderHtml $ docTypeHtml $ do
	H.head $ do
		title "GlobalDecls"
	body $ do
		h1 "GlobalDecls"
		h2 "Defs/Decls"
		showMapTable (render.pretty) (render.pretty) objs
		h2 "Tags"
		showMapTable (render.pretty) (render.pretty) tags
		h2 "Typedefs"
		showMapTable (render.pretty) (\ (TypeDef _ ty _ _) -> (render.pretty) ty) typedefs

astToHTMLString :: TranslUnit -> String
astToHTMLString ast = renderHtml $ docTypeHtml $ do
	H.head $ do
		title "AST"
	body $ do
		h1 "AST"
		showMapTable show show ast
