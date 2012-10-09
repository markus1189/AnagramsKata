import qualified Data.Map as M

import Data.List (sort)

wordlist :: IO [String]
wordlist = fmap lines $ readFile "wordlist.txt"

type AnagramMap = M.Map String [String]

insert :: AnagramMap -> String -> AnagramMap
insert mp str = M.insertWith (++) (sort str) [str] mp

buildMap :: [String] -> AnagramMap
buildMap = foldl insert M.empty

main :: IO ()
main = do
  anagrams <- fmap (M.elems . buildMap) wordlist
  mapM_ print anagrams
