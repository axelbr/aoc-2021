
import System.IO
import System.Environment
import Lib (Coordinate (..), Command (..), applyPlan, getPosition, getDepth)

fromString :: String -> String -> Command
fromString "forward" arg = Forward (read arg)
fromString "down" arg = Down (read arg)
fromString "up" arg = Up (read arg)
fromString invalid arg = error "Invalid command"

parse :: String -> Command
parse arg
    | length args == 2 = fromString (head args) (last args)
    | otherwise = error "Invalid arguments"
    where args = words arg

main :: IO ()
main = do
    args <- getArgs
    filecontent <- readFile (head args)
    let result = applyPlan (Coordinate 0 0 0) $ map parse $ lines filecontent
    print result
    print (getPosition result * getDepth result)