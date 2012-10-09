import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

getWordlist = fmap lines . readFile
insert mp str = M.insertWith (++) (S.fromList str) [str] mp
buildMap = foldl insert M.empty
failWithMsg msg = putStrLn msg >> exitFailure
main = do
  args <- getArgs
  when (length args /= 1) (failWithMsg  "Syntax: anagrams wordlist")
  fmap (mapM_ print . M.elems . buildMap) . getWordlist . head $ args
