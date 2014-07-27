module Rules where

import Types
import Moore
import Control.Applicative
import qualified Data.List as L
import qualified Data.IntMap as I

-- | Germs with less friends than enemies get (eaten and) turned into enemies.
strengthInNumbers :: Rule
strengthInNumbers m = do
    Cell g  <- getGermCenter m
    (e, ec) <- strongestGerm m
    let fc = countGermsOfType g m
    return $ if ec > fc
               then changeCenter m $ Cell e
               else m

-- | Returns the number of the given germ in the neighborhood.
countGermsOfType :: Int -> Moore -> Int
countGermsOfType g = length . filter (== Cell g) . mooreToList

-- | Returns the germ with the most friends in the neighborhood.
strongestGerm :: Moore -> Maybe (Int, Int)
strongestGerm = safeHead . reverse . L.sortBy (\(_,a) (_,b) -> compare a b) . countGerms
    where safeHead (a:_) = Just a
          safeHead []    = Nothing

-- | Counts the amount of each germ in the neighborhood.
-- Given a result of (g, i), `g` is the germ's index and `i` is the amount.
countGerms :: Moore -> [(Int, Int)]
countGerms = count . filterGerms . mooreToList

-- | Count like occurences in a list of Ints.
count :: [Int] -> [(Int, Int)]
count = I.toList . foldl succOrInsert I.empty
    where succOrInsert im i = I.insert i (value i im) im
          value i im = case I.lookup i im of
                           Just v  -> v + 1
                           Nothing -> 1

-- | Take only cells that are germs.
-- Gather those germs by name (index).
filterGerms :: [Cell] -> [Int]
filterGerms = map (\(Cell i) -> i) . filter isGerm
    where isGerm (Cell _) = True
          isGerm _        = False

getGermCenter :: Moore -> Maybe Cell
getGermCenter m =
    case mooreCenter m of
        Cell g -> Just $ Cell g
        _      -> Nothing

