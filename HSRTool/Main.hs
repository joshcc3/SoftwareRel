import System.Environment 
import Data.List

main = do
   args <- getArgs
   progName <- getProgName
   fileContent <- readFile (args !! 0)
   putStrLn fileContent
