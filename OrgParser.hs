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


eol :: Parser ()
eol = ((char '\n') >> return ()) <|> eof

textline :: Parser String
textline = notFollowedBy (char '*') >> many (noneOf "\n")

text :: Parser String
text = unlines <$> sepEndBy (try textline) (try $ char '\n')

header :: Parser OrgHeader
header = OrgHeader
  <$> asts
  <*> ((char ' ') >> manyTill anyChar eol)
  <*> text

asts :: Parser Int
asts = length <$> many1 (try (char '*'))

orgfile :: Parser [OrgHeader]
orgfile = many header


data OrgTree = OrgTree OrgHeader [OrgTree]

struct :: [OrgHeader] -> [OrgTree]
struct [] = []
struct (h:hs) = OrgTree h (struct internal) : (struct reminder)
  where (internal,reminder) = break (<=h) hs

orgtree :: Parser [OrgTree]
orgtree = struct <$> orgfile

instance Show OrgTree where
  show = showTree

showTree :: OrgTree -> String
showTree (OrgTree head trees) = unlines $ map ("| " ++) $ lines $ unlines
  [ "## Title:" ++ title head
  , "~~ "
  , content head
  , concatMap showTree trees
  ]
