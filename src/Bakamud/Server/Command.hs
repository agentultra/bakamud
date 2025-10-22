{-# LANGUAGE OverloadedStrings #-}

module Bakamud.Server.Command where

import Data.Char
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Username = Username Text
  deriving (Eq, Ord, Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show (Password _) = "(Password ***)"

data Command
  = Login Username Password
  | Motd
  | Register Username Password
  | HandleParseError Text
  | Unprocessed Text
  deriving (Eq, Show)

type Parser = Parsec Void Text

commandP :: Parser Command
commandP
  = try loginCommandP
  <|> try registerCommandP
  <|> try motdCommandP
  <|> unprocessedCommandP

-- "login fooo pass"
loginCommandP :: Parser Command
loginCommandP = do
  _ <- string "login"
  _ <- spaceChar
  username <- takeWhile1P Nothing isAlphaNum
  _ <- spaceChar
  passwd <- takeWhile1P Nothing isPrint
  pure $ Login (Username username) (Password passwd)

motdCommandP :: Parser Command
motdCommandP = do
  _ <- string "motd"
  pure $ Motd

registerCommandP :: Parser Command
registerCommandP = do
  _ <- string "register"
  _ <- spaceChar
  username <- takeWhile1P Nothing isAlphaNum
  _ <- spaceChar
  password <- takeWhile1P Nothing isPrint
  pure $ Register (Username username) (Password password)

unprocessedCommandP :: Parser Command
unprocessedCommandP = do
  input <- takeWhile1P Nothing isPrint
  pure $ Unprocessed input
