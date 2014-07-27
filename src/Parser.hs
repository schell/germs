{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Types
import           Rules
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (take)
import Debug.Trace

game :: Parser Game
game = do
    defs <- definitions
    case defsToGame defs of
        Right g  -> return g
        Left err -> fail $ unwords ["Parsing Game failed:", err]

definitions :: Parser [Definition]
definitions = many1' (skipSpaceOrNewline *> comment *> skipSpaceOrNewline *> definition)

definition :: Parser Definition
definition = defBoard <|>
             defGerm  <|>
             defRule

defBoard :: Parser Definition
defBoard = DefBoard <$> board

defGerm :: Parser Definition
defGerm = DefGerm <$> germ <?> "Definition of Germ"

defRule :: Parser Definition
defRule = DefRule <$> rule <?> "Definition of Rule"

rule :: Parser Rule
rule = ruleStrengthInNumbers

ruleStrengthInNumbers :: Parser Rule
ruleStrengthInNumbers = strengthInNumbers <$ string "StrengthInNumbers"

comment :: Parser String
comment = string "--" *> manyTill anyChar endOfLine <?> "Comment"

board :: Parser Board
board = do
    grid <- boardGrid
    case trace (show grid) grid of
        (r:rs) -> if (all (== length r) $ map length rs)
                    then return $ Board (length r) (concat grid)
                    else fail "Board has rows of unequal length"
        _      -> fail "Board has no rows."

boardGrid :: Parser [[Cell]]
boardGrid = many1' boardRow

boardRow :: Parser [Cell]
boardRow =  many1' cell <* endOfLine

cell :: Parser Cell
cell = cgerm <|> rock <|> empty' <?> "Cell"

cgerm :: Parser Cell
cgerm = do{i <- digit; return $ Cell (read [i])} <?> "Germ Cell"

rock :: Parser Cell
rock = Rock <$ char '#' <?> "Rock"

empty' :: Parser Cell
empty' = Empty <$ (char ' ' <|> char '_') <?> "Empty"

oneGerm :: Parser Germ
oneGerm = germ <* endOfInput <?> "One Germ"

germ :: Parser Germ
germ = many1' transition <?> "Germ"

transition :: Parser Transition
transition =
    (north     <|>
     northeast <|>
     east      <|>
     southeast <|>
     south     <|>
     southwest <|>
     west      <|>
     northwest <|>
     stay      <|>
     die
    ) <?> "Transition"

north :: Parser Transition
north = N <$> tcell <*  move  <*> tcell <* skipSpaceOrNewline
          <*> tcell <*  self  <*> tcell <* skipSpaceOrNewline
          <*> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
          <?> "Transition North"

northeast :: Parser Transition
northeast = NE <$> tcell <*> tcell <*  move   <* skipSpaceOrNewline
               <*> tcell <*  self  <*> tcell  <* skipSpaceOrNewline
               <*> tcell <*> tcell <*> tcell  <* skipSpaceOrNewline
            <?> "Transition NorthEast"

east :: Parser Transition
east = E <$> tcell <*> tcell <*> tcell  <* skipSpaceOrNewline
         <*> tcell <*  self  <*  move   <* skipSpaceOrNewline
         <*> tcell <*> tcell <*> tcell  <* skipSpaceOrNewline
         <?> "Transition East"

southeast :: Parser Transition
southeast = SE <$> tcell <*> tcell <*> tcell  <* skipSpaceOrNewline
               <*> tcell <*  self  <*> tcell  <* skipSpaceOrNewline
               <*> tcell <*> tcell <*  move   <* skipSpaceOrNewline
               <?> "Transition SouthEast"

south :: Parser Transition
south = S <$> tcell <*> tcell <*> tcell  <* skipSpaceOrNewline
          <*> tcell <*  self  <*> tcell  <* skipSpaceOrNewline
          <*> tcell <*  move  <*> tcell  <* skipSpaceOrNewline
          <?> "Transition South"

southwest :: Parser Transition
southwest = SW <$> tcell <*> tcell <*> tcell  <* skipSpaceOrNewline
               <*> tcell <*  self  <*> tcell  <* skipSpaceOrNewline
               <*  move  <*> tcell <*> tcell  <* skipSpaceOrNewline
               <?> "Transition SouthWest"

west :: Parser Transition
west = W <$> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
         <*  move  <*  self  <*> tcell <* skipSpaceOrNewline
         <*> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
         <?> "Transition West"

northwest :: Parser Transition
northwest = W <$  move  <*> tcell <*> tcell <* skipSpaceOrNewline
              <*> tcell <*  self  <*> tcell <* skipSpaceOrNewline
              <*> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
              <?> "Transition NorthWest"

stay :: Parser Transition
stay = Stay <$> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
            <*> tcell <*  self  <*> tcell <* skipSpaceOrNewline
            <*> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
            <?> "Transition Stay"

die :: Parser Transition
die = Die <$> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
          <*> tcell <*  tempty<*> tcell <* skipSpaceOrNewline
          <*> tcell <*> tcell <*> tcell <* skipSpaceOrNewline
          <?> "Transition Death"

skipSpaceOrNewline :: Parser ()
skipSpaceOrNewline = skipWhile isCruft
    where isCruft c = or $ map (== c) [' ', '\n', '\t']

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
