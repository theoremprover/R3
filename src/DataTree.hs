{-# LANGUAGE PackageImports,TypeOperators,FlexibleInstances,FlexibleContexts,ScopedTypeVariables,UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DataTree (genericToHTMLString,genericToString,dataTreeToHTMLString,DataTree(..)) where

import GHC.Generics
import "language-c" Language.C
import Language.C.Data.Ident
import Text.Html


data DataTree = DataTree String [DataTree] | Leaf String deriving (Show)

data Mode = None | CollectList deriving (Show,Eq)

class DataTreeNode f where
	dataTree :: Mode -> f p -> [DataTree]

instance DataTreeNode V1 where
	dataTree _ _ = error "DataTreeNode of empty type!"

instance {-# OVERLAPS #-} DataTreeNode (K1 i NodeInfo) where
	dataTree _ (K1 c) = [Leaf $ show c]

instance {-# OVERLAPS #-} DataTreeNode (K1 i Ident) where
	dataTree _ (K1 (Ident name i _)) = [Leaf $ "Ident " ++ show name ++ " " ++ show i]

instance {-# OVERLAPS #-} DataTreeNode (K1 i Int) where
	dataTree _ (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i String) where
	dataTree _ (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i Char) where
	dataTree _ (K1 c) = [Leaf $ show c]
instance {-# OVERLAPS #-} DataTreeNode (K1 i Integer) where
	dataTree _ (K1 c) = [Leaf $ show c]

instance (Generic c,DataTreeNode (Rep c)) => DataTreeNode (K1 i c) where
	dataTree mode (K1 c) = dataTree mode (from c)

instance (DataTreeNode f,Constructor c) => DataTreeNode (M1 C c f) where
	dataTree mode (M1 x) = case (mode,conName (undefined :: M1 C c f p)) of
		(None,       ":")  -> [DataTree "[]" (dataTree CollectList x)]
		(CollectList,":")  -> dataTree CollectList x
		(CollectList,"[]") -> []
		(_,conname)        -> [DataTree conname (dataTree None x)]

instance (DataTreeNode f,Selector s) => DataTreeNode (M1 S s f) where
	dataTree mode (M1 x) = dataTree mode x

instance (DataTreeNode f,Datatype d) => DataTreeNode (M1 D d f) where
	dataTree mode (M1 x) = dataTree mode x

instance DataTreeNode U1 where
	dataTree _ U1 = []

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :*: f2) where
	dataTree mode (a :*: b) = dataTree mode a ++ dataTree mode b

instance (DataTreeNode f1,DataTreeNode f2) => DataTreeNode (f1 :+: f2) where
	dataTree mode (L1 x) = dataTree mode x
	dataTree mode (R1 x) = dataTree mode x

toDataTree :: (Generic a,DataTreeNode (Rep a)) => a -> [DataTree]
toDataTree x = dataTree None (from x)

dataTreeToString ind (Leaf s) = indent ind ++ s ++ "\n" where
	indent x = concat $ replicate ind "|   "
dataTreeToString ind (DataTree s subtrees) = dataTreeToString ind (Leaf s) ++
	concatMap (dataTreeToString (ind+1)) subtrees

dataTreeToHtml (Leaf s) = li (stringToHtml s)
dataTreeToHtml (DataTree s subtrees) = (li ! [identifier "myUL"]) ((thespan ! [myclass]) (stringToHtml s) +++
	(ulist ! [theclass "nested"]) (concatHtml $ map dataTreeToHtml subtrees))
	where
	myclass = theclass $ if null subtrees then "leaf" else "caret"

genericToString :: (Generic a,DataTreeNode (Rep a)) => a -> String
genericToString x = concatMap (dataTreeToString 0) (toDataTree x)

genericToHTMLString :: (Generic a,DataTreeNode (Rep a)) => a -> String
genericToHTMLString x = dataTreeToHTMLString $ toDataTree x

dataTreeToHTMLString :: [DataTree] -> String
dataTreeToHTMLString datatrees = renderHtml $ pageframe $ map dataTreeToHtml datatrees
	where
	chkbox = primHtml "<input type=CHECKBOX id=cbtoggleexpand onclick=toggleexpand()>Expand all</input>"
	pageframe tree_htmls =
		header (style css) +++
		body ( concatHtml (chkbox : tree_htmls) +++ tag "script" toggler +++ tag "script" expandall )

css = primHtml "\
\ ul, #myUL {\
\  list-style-type: none;\
\ }\
\ \
\ #myUL {\
\  margin: 0;\
\  padding: 0;\
\ }\
\ \
\ .caret {\
\  cursor: pointer;\
\  user-select: none;\
\ }\
\ \
\ .caret::before {\
\  content: \"\\229E\";\
\  color: #808080;\
\  display: inline-block;\
\  margin-right: 6px;\
\ }\
\ \
\ .caret-down::before {\
\  content: \"\\229F\";\
\  color: #808080;\
\ }\
\ \
\ .leaf {\
\  cursor: pointer;\
\  user-select: none;\
\ }\
\ \
\ .leaf::before {\
\  content: \"\\22A1\";\
\  visibility: hidden;\
\  color: #808080;\
\  display: inline-block;\
\  margin-right: 6px;\
\ }\
\ \
\ .leaf-down::before {\
\  content: \"\\22A1\";\
\  visibility: hidden;\
\  color: #808080;\
\ }\
\ \
\ .nested {\
\  display: none;\
\  margin: 0;\
\ }\
\ \
\ .active {\
\  display: block;\
\  margin: 0;\
\ }\
\ "

toggler = primHtml "\
\ var toggler = document.getElementsByClassName(\"caret\");\
\ var i;\
\ \
\ for (i = 0; i < toggler.length; i++) {\
\  toggler[i].addEventListener(\"click\", function() {\
\    this.parentElement.querySelector(\".nested\").classList.toggle(\"active\");\
\    this.classList.toggle(\"caret-down\");\
\  });\
\ }"

expandall = primHtml "\
\ function toggleexpand() {\
\   var checkBox = document.getElementById(\"cbtoggleexpand\");\
\   if (checkBox.checked == true){\
\     document.querySelectorAll(\".nested\").forEach(function(elt) { elt.classList.add(\"active\"); } );\
\     document.querySelectorAll(\".caret\").forEach(function(elt) {elt.classList.add(\"caret-down\");} );\
\   } else {\
\     document.querySelectorAll(\".nested\").forEach(function(elt) { elt.classList.remove(\"active\"); } );\
\     document.querySelectorAll(\".caret\").forEach(function(elt) {elt.classList.remove(\"caret-down\");} );\
\   }\
\ }\
\ "