module Parser where

import           Types
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (take)

board :: Int -> Parser Board
board i = Board i <$> many' cell

cell :: Parser Cell
cell = cgerm <|> rock <|> empty'

cgerm :: Parser Cell
cgerm = Cell <$> decimal

rock :: Parser Cell
rock = Rock <$ char '#'

empty' :: Parser Cell
empty' = Empty <$ char ' '

oneGerm :: Parser Germ
oneGerm = germ <* endOfInput

germ :: Parser Germ
germ = many' transition <?> "Germ"

transition :: Parser Transition
transition = north <|> northeast <|> east <|> southeast <|> south <|> southwest <|> west <|> die <?> "Transition"

north :: Parser Transition
north = N <$> tcell <*  move  <*> tcell
          <*> tcell <*  self  <*> tcell
          <*> tcell <*> tcell <*> tcell
          <?> "Transition North"

northeast :: Parser Transition
northeast = NE <$> tcell <*> tcell <*  move
               <*> tcell <*  self  <*> tcell
               <*> tcell <*> tcell <*> tcell
            <?> "Transition NorthEast"

east :: Parser Transition
east = E <$> tcell <*> tcell <*> tcell
         <*> tcell <*  self  <*  move
         <*> tcell <*> tcell <*> tcell
         <?> "Transition East"

southeast :: Parser Transition
southeast = SE <$> tcell <*> tcell <*> tcell
               <*> tcell <*  self  <*> tcell
               <*> tcell <*> tcell <*  move
               <?> "Transition SouthEast"

south :: Parser Transition
south = S <$> tcell <*> tcell <*> tcell
          <*> tcell <*  self  <*> tcell
          <*> tcell <*  move  <*> tcell
          <?> "Transition South"

southwest :: Parser Transition
southwest = SW <$> tcell <*> tcell <*> tcell
               <*> tcell <*  self  <*> tcell
               <*  move  <*> tcell <*> tcell
               <?> "Transition SouthWest"

west :: Parser Transition
west = W <$> tcell <*> tcell <*> tcell
         <*  move  <*  self  <*> tcell
         <*> tcell <*> tcell <*> tcell
         <?> "Transition West"

die :: Parser Transition
die = Die <$> tcell <*> tcell <*> tcell
          <*> tcell <*  tempty<*> tcell
          <*> tcell <*> tcell <*> tcell
          <?> "Transition Death"

tcell :: Parser TCell
tcell = trock <|> tfood <|> tempty <|> self <|> wild <?> "TCell"

trock :: Parser TCell
trock = TRock <$ char '0' <?> "TCell Rock"

tfood :: Parser TCell
tfood = TFood <$ char '$' <?> "TCell Food"

tempty :: Parser TCell
tempty = TEmpty <$ char ' ' <?> "TCell Empty"

self :: Parser TCell
self = TSelf <$ char 'X' <?> "TCell Self"

wild :: Parser TCell
wild = TWild <$ char '*' <?> "TCell Wildcard"

move :: Parser Char
move = char '^' <?> "TCell Movement"
