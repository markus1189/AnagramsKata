import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

getWordlist :: FilePath -> IO [String]
getWordlist = fmap lines . readFile

type AnagramMap = M.Map (S.Set Char) [String]

insert :: AnagramMap -> String -> AnagramMap
insert mp str = M.insertWith (++) (S.fromList str) [str] mp

buildMap :: [String] -> AnagramMap
buildMap = foldl insert M.empty

failWithMsg :: String -> IO ()
failWithMsg msg = putStrLn msg >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (failWithMsg  "Syntax: anagrams wordlist")
  wordList <- getWordlist . head $ args
  let anagrams = M.elems . buildMap $ wordList
  mapM_ print anagrams
