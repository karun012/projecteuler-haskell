import Test.DocTest
import System.Directory
import Data.List

main :: IO ()
main = getSourceFiles >>= \sourceFiles -> doctest $
      "-isrc" : sourceFiles

getSourceFiles :: IO [FilePath]
getSourceFiles = do 
                files <- getDirectoryContents "src"
                let haskellSourceFiles = (filter (`notElem` [".", ".."]) . filter (not . isSuffixOf ".hs")) files
                return haskellSourceFiles


