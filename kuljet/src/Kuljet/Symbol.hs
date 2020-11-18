module Kuljet.Symbol where

import qualified Data.Text as T


data Symbol =
  Symbol { symbolName :: T.Text }
  deriving (Show, Eq, Ord)
