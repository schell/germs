{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Types
import           Parser
import           System.Exit
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString.Char8 as B


main :: IO ()
main = do
    putStrLn "GERMS!"
    let eGerm = readGerm dna
        dna   = concat [ "$^*"
                       , "*X*"
                       , "***"

                       , "**$"
                       , "*X^"
                       , "***"

                       , "***"
                       , "*X*"
                       , "*^$"

                       , "***"
                       , "^X*"
                       , "$**"
                       ]
    putStrLn $ "Parsing germ dna: " ++ dna
    g <- case eGerm of
             Left err -> putStrLn err >> exitFailure
             Right g  -> do putStrLn $ unwords ["Read germ:", show g]
                            return g
    txt <- readFile "board.txt"

    b <- case readBoard txt of
             Left err -> putStrLn err >> exitFailure
             Right b  -> do print b
                            return b
    return ()

readBoard :: String -> Either String Board
readBoard b =
    let width  = length $ head $ lines b
    in parseOnly (board width) $ B.pack $ concat $ lines b

readGerm :: String -> Either String Germ
readGerm dna =
    let rGerm = parse oneGerm $ B.pack dna
        read' rg = case rg of
                       Fail i ctxs err -> Left $ unwords ["error", err, "about (", unwords ctxs, ") at", B.unpack i]
                       Partial p       -> read' (p "")
                       Done _ g        -> Right g
    in read' rGerm
