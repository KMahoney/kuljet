module Kuljet.PathPattern where

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import Kuljet.Symbol

data Path =
  Path { pathSegments :: [PathSegment] }
  deriving (Show)

data PathSegment
  = PathMatch T.Text
  | PathVar Symbol
  deriving (Show)

pathVars :: Path -> [Symbol]
pathVars (Path segments) =
  Maybe.mapMaybe symName segments

  where
    symName =
      \case
        PathVar sym -> Just sym
        _ -> Nothing


type PathVars = M.Map Symbol T.Text


matchPath :: Path -> [T.Text] -> Maybe PathVars
matchPath (Path matchSegments) givenSegments =
  matches M.empty matchSegments givenSegments

  where
    matches :: M.Map Symbol T.Text -> [PathSegment] -> [T.Text] -> Maybe (M.Map Symbol T.Text)
    matches matchValues [] [] = Just matchValues
    matches _ _ [] = Nothing
    matches _ [] _ = Nothing
    matches matchValues (m : ms) (s : ss) =
      case m of
        PathMatch text ->
          if text == s
          then matches matchValues ms ss
          else Nothing
        PathVar sym ->
          matches (M.insert sym s matchValues) ms ss
