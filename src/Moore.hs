module Moore where

import           Types
import           Data.List
import           Data.Maybe
import qualified Data.IntMap as I
import           Control.Applicative
import Debug.Trace
import Text.Printf

stepMoore :: Moore -> Germ -> Moore
stepMoore m [] = m
stepMoore m (t:ts) = fromMaybe (stepMoore m ts) (applyTransition t m)

applyTransition :: Transition -> Moore -> Maybe Moore
applyTransition tn m =
    let pre = preTransitionToMooreList tn
        pst = postTransitionToMooreList tn
        ml  = mooreToList m
    in case mooreCenter m of
        Cell i -> if all (uncurry (matchesCell i)) $ zip pre ml
                    then listToMoore $ zipWith (transitionCell i) pst ml
                    else Nothing
        _      -> Nothing

matchMoore :: Moore -> I.IntMap Germ -> Maybe Germ
matchMoore m gm =
    case mooreCenter m of
        Cell i -> I.lookup i gm
        _      -> Nothing

cellsToMoores :: Int -> [Cell] -> [Moore]
cellsToMoores w cs = map (\i -> getMoore i w cs) (getIndices 0 0 w h)
    where h  = floor $ ((fromIntegral $ length cs) / fromIntegral w :: Double)

cellsToMoores' :: Int -> [Cell] -> [Moore]
cellsToMoores' w cs = zipCells rbs $
                      zipCells xbs $
                      zipCells lbs $
                      zipCells rys $
                      zipCells xys $
                      zipCells lys $
                      zipCells rts $
                      zipCells xts $
                      zipCells lts ms
    where zipCells is fs = zipWith ($) fs ((cs !!) <$> is)
          h   = floor $ ((fromIntegral $ length cs) / fromIntegral w :: Double)
          ms  = replicate (length lts) Moore
          lts = getIndices 0 0 w h
          xts = getIndices 1 0 w h
          rts = getIndices 2 0 w h
          lys = getIndices 0 1 w h
          xys = getIndices 1 1 w h
          rys = getIndices 2 1 w h
          lbs = getIndices 0 2 w h
          xbs = getIndices 1 2 w h
          rbs = getIndices 2 2 w h

mooresToCells :: Int -> [Moore] -> [Cell]
mooresToCells w ms = rows
    where mgrid   = ms `toRowsOf` (floor $ (fromIntegral w/3 :: Double)) :: [[Moore]]
          rows    = concatMap (toRow . to3Rows) mgrid :: [Cell]
          toRow (a,b,c) = a ++ b ++ c

to3Rows :: [Moore] -> ([Cell], [Cell], [Cell])
to3Rows = foldl accMoore ([],[],[])
    where accMoore (t,m,b') (Moore a b c d e f g h i) = (t++[a,b,c], m++[d,e,f], b'++[g,h,i])

getIndices :: Int -> Int -> Int -> Int -> [Int]
getIndices xi yi w h = [ y*w + x | y <- [yi,yi+3..h-(3-yi)], x <- [xi,xi+3..w-(3-xi)] ]

getMoore :: Int -> Int -> [Cell] -> Moore
getMoore i w cs =
    Moore (cs !! lt) (cs !! xt) (cs !! rt)
          (cs !! ly) (cs !! xy) (cs !! ry)
          (cs !! lb) (cs !! xb) (cs !! rb)
      where ( (lt,xt,rt)
             ,(ly,xy,ry)
             ,(lb,xb,rb)) = mooreIndicesAt i w $ length cs

putMoore :: Moore -> Int -> Board -> Board
putMoore m i brd = brd{ boardCells = cells }
      where cells = foldl (\acc (i', c) -> put i' c acc) cs zipd
            cs    = boardCells brd
            zipd  = zip [lt,xt,rt,ly,xy,ry,lb,xb,rb] (mooreToList m)
            put i' a as = take i' as ++ [a] ++ drop (i'+1) as
            ( (lt,xt,rt)
             ,(ly,xy,ry)
             ,(lb,xb,rb)) = mooreIndicesAt i (boardWidth brd) (length $ boardCells brd)

printGrid :: PrintfArg a => [[a]] -> IO ()
printGrid = putStrLn . unlines . map (unwords . map (printf "%3i"))

toRowsOf :: [a] -> Int -> [[a]]
toRowsOf [] _ = []
toRowsOf as i = [take i as] ++ drop i as `toRowsOf` i

toColumnsOf :: [a] -> Int -> [[a]]
toColumnsOf as i = transpose $ toRowsOf as i

cycleGridRows :: [[a]] -> [[a]]
cycleGridRows as = drop 1 as ++ take 1 as

uncycleGridRows :: [[a]] -> [[a]]
uncycleGridRows as = [last as] ++ take (length as -1) as

cycleGridCols :: [[a]] -> [[a]]
cycleGridCols = transpose . cycleGridRows . transpose

uncycleGridCols :: [[a]] -> [[a]]
uncycleGridCols = transpose . uncycleGridRows . transpose

cycleGrid :: [[a]] -> [[a]]
cycleGrid = cycleGridCols . cycleGridRows

mooreCenter :: Moore -> Cell
mooreCenter (Moore _ _ _
                   _ c _
                   _ _ _) = c

changeCenter :: Moore -> Cell -> Moore
changeCenter (Moore a b c
                    d _ f
                    g h i) e = Moore a b c d e f g h i

mooreToList :: Moore -> [Cell]
mooreToList (Moore a b c
                   d e f
                   g h i) = [a,b,c,d,e,f,g,h,i]

listToMoore :: [Cell] -> Maybe Moore
listToMoore (a:b:c:d:e:f:g:h:i:_) = Just $ Moore a b c d e f g h i
listToMoore _ = Nothing

mooreIndicesAt :: Int -> Int -> Int -> ((Int,Int,Int),(Int,Int,Int),(Int,Int,Int))
mooreIndicesAt i w' len =
    ( (t'*w + l', t'*w + x, t'*w + r')
    , (y *w + l', y *w + x, y *w + r')
    , (b'*w + l', b'*w + x, b'*w + r')
    ) where (w,h,x,y) = asIf2d len i w'
            (l,r,t,b)  = (x - 1, x + 1, y - 1, y + 1)
            -- Wrap the board around (infinite map)
            l' = if l == -1 then w - 1 else l `mod` w
            r' = if r == w then 0 else r `mod` w
            t' = if t == -1 then h - 1 else t `mod` h
            b' = if b == h then 0 else b `mod` h

asIf2d :: Int -> Int -> Int -> (Int,Int,Int,Int)
asIf2d len i w = (w,h,x,y)
    where h  = floor (fromIntegral len / fromIntegral w :: Double)
          x  = i `mod` w
          y  = floor (fromIntegral i / fromIntegral w :: Double)

