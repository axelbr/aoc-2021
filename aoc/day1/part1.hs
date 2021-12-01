
import System.IO
import System.Environment
import Lib (detectMeasurementEdge, countIncreases)

main :: IO ()
main = do
    args <- getArgs  
    filecontent <- readFile (head args)
    print (countIncreases $ detectMeasurementEdge $ map read $ lines filecontent)