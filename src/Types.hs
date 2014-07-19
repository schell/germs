module Types where

import qualified Data.IntMap as I


data Game = Game { gameBoard :: Board
                 , gameGerms :: I.IntMap Germ
                 } deriving (Show)

instance Show Board where
    show (Board _ []) = ""
    show (Board w cells) = unwords ["Board"
                                   , show w
                                   ,"\n"
                                   , concatMap show cells
                                   ]


data Board = Board { boardWidth :: Int
                   , boardCells :: [Cell]
                   }

instance Show Cell where
    show Rock = "#"
    show (Cell i) = show i
    show Empty = " "

data Cell = Cell Int
          | Rock
          | Empty

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
    show (Die a b c d f g h i) = inject 4 'X' $ concatMap show [a,b,c,d,f,g,h,i]

inject :: Int -> a -> [a] -> [a]
inject i a as
    | i > (length as - 1) = as
    | otherwise = take i as ++ [a] ++ drop i as

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

