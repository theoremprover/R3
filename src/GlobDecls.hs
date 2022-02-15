{-# LANGUAGE PackageImports,OverloadedStrings,RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module GlobDecls where

import Language.C.Analysis.SemRep
import Text.Blaze.Html4.Strict as H
import Text.Blaze.Html.Renderer.String
import Control.Monad
import Data.Map.Strict as Map

--import Prettyptrinter
--import Prettyprinter.Render.Text

import AST

showMapTable :: (Show k, Show v) => Map.Map k v -> Html
showMapTable mapping = table $
	forM_ (assocs mapping) $ \ (key,val) -> tr $ do
		td $ toHtml $ show key
		td $ preEscapedToHtml ("&#x21a6;"::String)
		td $ toHtml $ show val

globdeclsToHTMLString :: GlobalDecls -> String
globdeclsToHTMLString (GlobalDecls objs tags typedefs) = renderHtml $ docTypeHtml $ do
	H.head $ do
		title "GlobalDecls"
	body $ do
		h1 "GlobalDecls"
		h2 "Defs/Decls"
		showMapTable objs
		h2 "Tags"
		showMapTable tags
		h2 "Typedefs"
		showMapTable  typedefs

astToHTMLString :: TranslUnit -> String
astToHTMLString ast = renderHtml $ docTypeHtml $ do
	H.head $ do
		title "AST"
	body $ do
		h1 "AST"
		showMapTable ast
