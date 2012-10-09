import qualified Data.Map as M
import qualified Data.Set as S

wordlist :: IO [String]
wordlist = fmap lines $ readFile "wordlist.txt"

type AnagramMap = M.Map (S.Set Char) [String]

insert :: AnagramMap -> String -> AnagramMap
insert mp str = M.insertWith (++) (S.fromList str) [str] mp

buildMap :: [String] -> AnagramMap
buildMap = foldl insert M.empty

main :: IO ()
main = do
  anagrams <- fmap (M.elems . buildMap) wordlist
  mapM_ print anagrams
