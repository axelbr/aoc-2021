module Lib (
    Coordinate (..),
    Command (..),
    apply, applyPlan, getPosition, getDepth) where
import GHC.Natural (Natural)

data Coordinate = Coordinate Natural Natural Natural deriving (Show)

data Command = Forward Natural | Down Natural | Up Natural deriving (Show)

getPosition :: Coordinate -> Natural
getPosition (Coordinate pos depth aim) = pos

getDepth :: Coordinate -> Natural
getDepth (Coordinate pos depth aim) = depth

apply :: Coordinate -> Command -> Coordinate
apply (Coordinate pos depth aim) (Forward x) = Coordinate (pos+x) (depth + x * aim) aim
apply (Coordinate pos depth aim) (Down x) = Coordinate pos depth (aim + x)
apply (Coordinate pos depth aim) (Up x) = Coordinate pos depth (aim - x)

applyPlan :: Coordinate -> [Command] -> Coordinate
applyPlan = foldl apply