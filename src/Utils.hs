{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Utils (
	module Utils,
	module Control.Monad.Extra)
	where

import Control.Monad.Extra

for :: [a] → (a → b) → [b]
for = flip map
