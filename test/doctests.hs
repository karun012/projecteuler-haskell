import Test.DocTest
import System.Directory
import Data.List
import System.FilePath.Posix

main :: IO ()
main = getSourceFiles >>= \sourceFiles -> doctest $
      "-isrc" : sourceFiles

getSourceFiles :: IO [FilePath]
getSourceFiles = do 
                files <- getDirectoryContents "src"
                let haskellSourceFiles = (map dropExtension . filter (`notElem` [".", ".."]) . filter (isSuffixOf ".hs")) files
                return haskellSourceFiles
