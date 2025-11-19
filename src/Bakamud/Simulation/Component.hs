{-# LANGUAGE OverloadedStrings #-}

module Bakamud.Simulation.Component where

import Control.Error.Util
import Data.Char
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data DType = DInt | DNum | DString deriving (Eq, Show)

newtype Definition = Definition (Map Text DType)
  deriving (Eq, Show)

data Value = VInt Int | VNum Float | VString Text deriving (Eq, Show)

type ComponentValues = Map Text Value

newtype Component = Component (Map Text Value)
  deriving (Eq, Show)

data TypeCheck = Match | Fail deriving (Eq, Show)

typeCheck :: DType -> Value -> TypeCheck
typeCheck DInt (VInt _) = Match
typeCheck DNum (VNum _) = Match
typeCheck DString (VString _) = Match
typeCheck _ _ = Fail

instantiate :: Definition -> ComponentValues -> Either Text Component
instantiate (Definition definitions) componentValues = do
  instantiatedValues <- (`Map.traverseWithKey` componentValues) $ \key val -> do
    defType <- note "Missing definition key" $ Map.lookup key definitions
    case typeCheck defType val of
      Fail -> Left "Invalid type for instantiation"
      Match -> pure val
  pure $ Component instantiatedValues

type Parser = Parsec Void Text

definitionP :: Parser (Text, Definition)
definitionP = do
  skipMany space
  labelStr <- someTill (satisfy isAlphaNum) eol
  skipMany space
  _ <- string "{"
  defs <- manyTill defP endDefP

  pure
    ( Text.pack labelStr
    , Definition $ Map.fromList defs
    )

defP :: Parser (Text, DType)
defP = undefined

endDefP :: Parser ()
endDefP = undefined
