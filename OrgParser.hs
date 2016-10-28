{-|
Module: OrgParser
Author: Mario Román
Description: A parser of the markup-related subset of org-mode
License: GPL-3

This module implements a set of parsers of the markup syntax of org-mode.
It is not a complete implementation of a org-mode parser. It only parses
certain syntax constructs mostly related to its internal markdown language.
-}
module OrgParser
  ( OrgHeader(..)
  , OrgTree(..)
  , orgtree
  ) where

import Text.ParserCombinators.Parsec
import Prelude hiding (head)

data OrgHeader = OrgHeader { level :: Int
                           , title :: String
                           , content :: String}
  deriving (Show)

instance Eq OrgHeader where
  (==) (OrgHeader a _ _) (OrgHeader b _ _) = a == b
instance Ord OrgHeader where
  compare (OrgHeader a _ _) (OrgHeader b _ _) = compare a b

-- | Parses end of line or end of file
eol :: Parser ()
eol = ((char '\n') >> return ()) <|> eof

-- | Parses a line that is NOT a header
textline :: Parser String
textline = notFollowedBy (char '*') >> many (noneOf "\n")

-- | Parses a set of lines, where none of them are headers
text :: Parser String
text = unlines <$> sepEndBy (try textline) (try $ char '\n')

-- | Parses an org-mode header
header :: Parser OrgHeader
header = OrgHeader
  <$> asts
  <*> ((char ' ') >> manyTill anyChar eol)
  <*> text

-- | Counts asterisks
asts :: Parser Int
asts = length <$> many1 (try (char '*'))

-- | Parses a complete org-file, dividing it in headers
orgfile :: Parser [OrgHeader]
orgfile = many header



data OrgTree = OrgTree OrgHeader [OrgTree]

instance Show OrgTree where
  show = showTree

-- | Given a list of headers, structures it into a tree, where
-- every header has a list of child headers.
struct :: [OrgHeader] -> [OrgTree]
struct [] = []
struct (h:hs) = OrgTree h (struct internal) : (struct reminder)
  where (internal,reminder) = break (<=h) hs

-- | Parses a complete org-file, giving it tree structure
orgtree :: Parser [OrgTree]
orgtree = struct <$> orgfile


showTree :: OrgTree -> String
showTree (OrgTree head trees) = unlines $ map ("| " ++) $ lines $ unlines
  [ "## Title:" ++ title head
  , "~~ "
  , content head
  , concatMap showTree trees
  ]

