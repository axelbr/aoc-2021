
import System.IO
import System.Environment
import Lib (detectMeasurementEdge, convolve, countIncreases)

main :: IO ()
main = do
    args <- getArgs  
    filecontent <- readFile (head args)
    print (countIncreases $ detectMeasurementEdge $ convolve 3 $ map read $ lines filecontent)