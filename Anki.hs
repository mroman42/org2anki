import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import OrgParser

textTrans :: (String -> String) -> OrgTree -> OrgTree
textTrans f (OrgTree (OrgHeader n t c) trees) = OrgTree (OrgHeader n t (f c)) (map (textTrans f) trees)

toDeck :: OrgTree -> String
toDeck (OrgTree _ trees) = concat $ map toSubdeck trees

toSubdeck :: OrgTree -> String
toSubdeck (OrgTree header trees) = concat $ map (((title header ++ ". ") ++) . toCard) trees

toCard :: OrgTree -> String
toCard (OrgTree header _) = concat [ title header
                                       , content header ]

toAnki :: OrgTree -> IO ()
toAnki tree@(OrgTree header _) = do
  writeFile (title header) (toDeck tree)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      result <- parse orgtree "" <$> readFile filename
      case result of
        Left err -> print err
        Right tree -> mapM_ toAnki tree
    _ -> putStrLn "Error. You must indicate a file."
