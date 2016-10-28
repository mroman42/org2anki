import Text.ParserCombinators.Parsec
import System.Environment
import Data.List
import OrgParser


ankinizeContent :: String -> String
ankinizeContent = (intercalate " ") . lines


toDeck :: OrgTree -> String
toDeck (OrgTree _ trees) = concat $ map toSubdeck trees

toSubdeck :: OrgTree -> String
toSubdeck (OrgTree header trees) = concat $ map (((title header ++ " ") ++) . toCard) trees

toCard :: OrgTree -> String
toCard (OrgTree header _) = (title header) ++ "\t" ++ (ankinizeContent $ content header) ++ "\n"

toAnki :: OrgTree -> IO ()
toAnki tree@(OrgTree header _) = do
  writeFile ("anki." ++ title header ++ ".csv") (toDeck tree)

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
