{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Types
import           Parser
import           Step
import           System.Exit
import           System.Environment
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8 hiding (take)
import qualified Data.ByteString.Char8 as B


main :: IO ()
main = do
    putStrLn "GERMS!"
    (f:_) <- getArgs
    txt <- readFile f

    ds <- case readDefinitions txt of
             Left err -> putStrLn err >> exitFailure
             Right ds  -> return ds
    putStrLn $ unwords ["Definitions:", show ds]

    g <- case defsToGame ds of
             Left err -> putStrLn err >> exitFailure
             Right g  -> return g

    --_ <- forkIO $ do _ <- getLine
    --                 exitSuccess
    loop g

loop :: Game -> IO ()
loop g = do
    let g' = stepGame g
        b  = gameBoard g
        b' = gameBoard g'
        s  = unlines $ zipWith (\l r -> l ++ pad l ++ " | " ++ r) (lines $ show b) (lines $ show b')
        pad a = replicate (boardWidth b - length a) ' '

    putStrLn s

    ch <- getChar
    when (ch == 'q') exitSuccess

    if b == b'
      then putStrLn $ unwords [ "Reached stasis after"
                              , show $ gameSteps g
                              , "steps."
                              ]
      else loop g'

readDefinitions :: String -> Either String [Definition]
readDefinitions = parseOnly definitions . B.pack

readGame :: String -> Either String Game
readGame = parseOnly game . B.pack

