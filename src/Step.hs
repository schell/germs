module Step where

import           Types
import           Moore
import           Control.Applicative
import           Data.Maybe
import qualified Data.IntMap as I
import Debug.Trace

stepGame :: Game -> Game
stepGame g = g{ gameBoard     = step (gameBoard g) (gameGerms g) (gameRules g)
              , gameSteps     = succ (gameSteps g)
              }

step :: Board -> I.IntMap Germ -> [Rule] -> Board
step b gm = stepRules (stepGerms b gm)

--stepGerms :: Board -> I.IntMap Germ -> Board
--stepGerms b gm = traverseBoard b (\i b' -> stepGerm i b' gm)

stepGerms :: Board -> I.IntMap Germ -> Board
stepGerms b gms = traverseMoores b (applyGerms gms)


applyGerms :: I.IntMap Germ -> Moore -> Moore
applyGerms gms m =
    let mb' = do germ <- matchMoore m gms
                 return $ stepMoore m germ
    in fromMaybe m mb'

--stepGerm :: Int -> Board -> I.IntMap Germ -> Board
--stepGerm i b gm = putMoore (applyGerm gm (getMoore i (boardWidth b) (boardCells b))) i b

stepRules :: Board -> [Rule] -> Board
stepRules b rs = traverseMoores b (applyRules rs)

applyRules :: [Rule] -> Moore -> Moore
applyRules [] m = m
applyRules (r:rs) m = applyRules rs (fromMaybe m $ r m)

traverseMoores :: Board -> (Moore -> Moore) -> Board
traverseMoores b f = b{ boardCells = traverseCells (boardWidth b) (boardCells b) f }

-- | Visits each cell's Moore neighborhood apply the Moore transition function
-- to each one exactly once.
traverseCells :: Int -> [Cell] -> (Moore -> Moore) -> [Cell]
traverseCells w cs f = uncycle $ hCycles $ vCycle $ hCycles $ vCycle $ hCycles cs
    where traverse cs' = mooresToCells w (f <$> cellsToMoores' w cs')
          traverseAndCycle = concat . cycleGridRows . (`toRowsOf` w) . traverse
          hCycles = head . reverse . take 4 . iterate traverseAndCycle
          vCycle  = concat . cycleGridCols . (`toRowsOf` w)
          uncycle = concat
                  . uncycleGridRows . uncycleGridRows . uncycleGridRows . uncycleGridCols
                  . uncycleGridRows . uncycleGridRows . uncycleGridRows . uncycleGridCols
                  . uncycleGridRows . uncycleGridRows . uncycleGridRows
                  . (`toRowsOf` w)

