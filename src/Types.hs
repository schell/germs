{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import qualified Data.IntMap as I

defsToGame :: [Definition] -> Either String Game
defsToGame = validateGame . foldl addDef emptyGame

addDef :: Game -> Definition -> Game
addDef g (DefRule r)  = g{ gameRules = (gameRules g) ++ [r] }
addDef g (DefBoard b) = g{ gameBoard = b }
addDef g (DefGerm gm) = let gms = gameGerms g
                            i   = I.size gms
                        in g{ gameGerms = I.insert i gm gms}

validateGame :: Game -> Either String Game
validateGame g =
    validateBoard (gameBoard g) >> validateGerms (gameGerms g) >> return g

validateBoard :: Board -> Either String Board
validateBoard b = validateCellsExist b >> validateCells b
    where w  = boardWidth b
          cs = boardCells b
          validateCellsExist b' = if length cs > 0
                                    then Right b'
                                    else Left "Board contains no cells"
          validateCells b' = if length cs `mod` w == 0
                               then Right b'
                               else Left "Board has funky cell row counts"

validateGerms :: I.IntMap Germ -> Either String (I.IntMap Germ)
validateGerms gs =
    if 1 <= I.size gs
      then Right gs
      else Left "Game must contain at least one Germ"

emptyGame :: Game
emptyGame = Game{ gameBoard = Board{ boardCells = []
                                   , boardWidth = 0
                                   }
                , gameGerms = I.empty
                , gameRules = []
                , gameSteps = 0
                }

instance Show Game where
    show (Game b gs rs s) = unwords [ "Game\n"
                                    , show b
                                    , "\n"
                                    , show gs
                                    , "\n"
                                    , show rs
                                    , "\n"
                                    , show s
                                    ]

data Game = Game { gameBoard :: Board
                 -- ^ All the cells in the world.
                 , gameGerms :: I.IntMap Germ
                 -- ^ A map of all our species of germs.
                 , gameRules :: [Rule]
                 -- ^ The world's environmental rules.
                 , gameSteps :: Int
                 -- ^ The current number of steps so far.
                 }

data Definition = DefRule  Rule
                | DefGerm  Germ
                | DefBoard Board
                deriving (Show)

instance Show Rule where
    show _ = "Rule"

type Rule = Moore -> Maybe Moore

data Moore = Moore Cell Cell Cell
                   Cell Cell Cell
                   Cell Cell Cell

instance Show Board where
    show (Board _ []) = ""
    show (Board w cells) = concat ["Board "
                                  , show w
                                  ,"\n"
                                  , unlines $ groupBy w $ concatMap show cells
                                  ]

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy i xs = take i xs:groupBy i (drop i xs)

data Board = Board { boardWidth :: Int
                   , boardCells :: [Cell]
                   } deriving (Eq)

instance Show Cell where
    show Rock = "#"
    show (Cell i) = show i
    show Empty = " "

transitionCell :: Int -> TCell -> Cell -> Cell
transitionCell i TSelf  Empty    = Cell i
transitionCell _ TEmpty (Cell _) = Empty
transitionCell _ _      cell     = cell

matchesCell :: Int -> TCell -> Cell -> Bool
matchesCell _ TRock  Rock     = True
matchesCell i TFood  (Cell n) = i /= n
matchesCell _ TEmpty Empty    = True
matchesCell i TSelf  (Cell n) = i == n
matchesCell _ TWild  _        = True
matchesCell _ _      _        = False

data Cell = Cell Int
          | Rock
          | Empty
          deriving (Eq)

type Germ = [Transition]

instance Show Transition where
    show (NW b c d f g h i)    = inject 4 'X' $ inject 0 '^' $ concatMap show [b,c,d,f,g,h,i]
    show (N a c d f g h i)     = inject 4 'X' $ inject 1 '^' $ concatMap show [a,c,d,f,g,h,i]
    show (NE a b d f g h i)    = inject 4 'X' $ inject 2 '^' $ concatMap show [a,b,d,f,g,h,i]
    show (W a b c f g h i)     = inject 4 'X' $ inject 3 '^' $ concatMap show [a,b,c,f,g,h,i]
    show (E a b c d g h i)     = inject 4 'X' $ inject 4 '^' $ concatMap show [a,b,c,d,g,h,i]
    show (SW a b c d f h i)    = inject 4 'X' $ inject 5 '^' $ concatMap show [a,b,c,d,f,h,i]
    show (S a b c d f g i)     = inject 4 'X' $ inject 6 '^' $ concatMap show [a,b,c,d,f,g,i]
    show (SE a b c d f g h)    = inject 4 'X' $ inject 7 '^' $ concatMap show [a,b,c,d,f,g,h]
    show (Die a b c d f g h i) = inject 4 ' ' $ concatMap show [a,b,c,d,f,g,h,i]
    show (Stay a b c d f g h i)= inject 4 'X' $ concatMap show [a,b,c,d,f,g,h,i]

inject :: Int -> a -> [a] -> [a]
inject i a as
    | i > (length as - 1) = as
    | otherwise = take i as ++ [a] ++ drop i as

preTransitionToMooreList :: Transition -> [TCell]
preTransitionToMooreList (NW b c d f g h i) = [TEmpty,      b,      c,      d, TSelf,      f,      g,      h,      i]
preTransitionToMooreList (N  a c d f g h i) = [     a, TEmpty,      c,      d, TSelf,      f,      g,      h,      i]
preTransitionToMooreList (NE a b d f g h i) = [     a,      b, TEmpty,      d, TSelf,      f,      g,      h,      i]
preTransitionToMooreList (W  a b c f g h i) = [     a,      b,      c, TEmpty, TSelf,      f,      g,      h,      i]
preTransitionToMooreList (E  a b c d g h i) = [     a,      b,      c,      d, TSelf, TEmpty,      g,      h,      i]
preTransitionToMooreList (SW a b c d f h i) = [     a,      b,      c,      d, TSelf,      f, TEmpty,      h,      i]
preTransitionToMooreList (S  a b c d f g i) = [     a,      b,      c,      d, TSelf,      f,      g, TEmpty,      i]
preTransitionToMooreList (SE a b c d f g h) = [     a,      b,      c,      d, TSelf,      f,      g,      h, TEmpty]
preTransitionToMooreList (Die a b c d f g h i) = [  a,      b,      c,      d, TSelf,      f,      g,      h,      i]
preTransitionToMooreList (Stay a b c d f g h i) = [ a,      b,      c,      d,  TSelf,      f,     g,      h,     i]

postTransitionToMooreList :: Transition -> [TCell]
postTransitionToMooreList (NW b c d f g h i) = [TSelf,      b,      c,      d,  TEmpty,     f,      g,     h,      i]
postTransitionToMooreList (N  a c d f g h i) = [     a, TSelf,      c,      d,  TEmpty,     f,      g,     h,      i]
postTransitionToMooreList (NE a b d f g h i) = [     a,      b, TSelf,      d,  TEmpty,     f,      g,     h,      i]
postTransitionToMooreList (W  a b c f g h i) = [     a,      b,      c, TSelf,  TEmpty,     f,      g,     h,      i]
postTransitionToMooreList (E  a b c d g h i) = [     a,      b,      c,      d, TEmpty, TSelf,      g,     h,      i]
postTransitionToMooreList (SW a b c d f h i) = [     a,      b,      c,      d, TEmpty,      f, TSelf,     h,      i]
postTransitionToMooreList (S  a b c d f g i) = [     a,      b,      c,      d, TEmpty,      f,     g, TSelf,      i]
postTransitionToMooreList (SE a b c d f g h) = [     a,      b,      c,      d, TEmpty,      f,     g,      h, TSelf]
postTransitionToMooreList (Die a b c d f g h i) = [  a,      b,      c,      d, TEmpty,      f,     g,      h,     i]
postTransitionToMooreList (Stay a b c d f g h i) = [ a,      b,      c,      d,  TSelf,      f,     g,      h,     i]


-- | The player can write rules for moving the cell
-- in a cardinal or ordinal direction.
data Transition = N TCell {-n-} TCell
                    TCell {-X-} TCell
                    TCell TCell TCell

                | NE TCell TCell {-ne-}
                     TCell {-X-} TCell
                     TCell TCell TCell

                | E TCell TCell TCell
                    TCell {-X-} {-e-}
                    TCell TCell TCell

                | SE TCell TCell TCell
                     TCell {-X-} TCell
                     TCell TCell {-se-}

                | S TCell TCell TCell
                    TCell {-X-} TCell
                    TCell {-s-} TCell

                | SW TCell  TCell TCell
                     TCell  {-X-} TCell
                     {-sw-} TCell TCell

                | W TCell  TCell TCell
                    {-w-}  {-X-} TCell
                    TCell  TCell TCell

                | NW {-nw-} TCell TCell
                     TCell  {-X-} TCell
                     TCell  TCell TCell

                | Stay TCell TCell TCell
                       TCell {-X-} TCell
                       TCell TCell TCell

                | Die TCell TCell TCell
                      TCell {-X-} TCell
                      TCell TCell TCell

instance Show TCell where
    show TRock  = "0"
    show TFood  = "$"
    show TEmpty = " "
    show TSelf  = "X"
    show TWild  = "*"

data TCell = TRock
           | TFood
           | TEmpty
           | TSelf
           | TWild
