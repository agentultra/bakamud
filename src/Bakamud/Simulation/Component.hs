{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bakamud.Simulation.Component where

import Control.Error.Util
import Data.Char (isPunctuation)
import Data.Map.Strict (Map)
import Data.Text (Text, dropAround)
import qualified Data.Map.Strict as Map
import qualified Language.Lua.Annotated.Lexer as Lua
import qualified Language.Lua.Parser as Lua
import qualified Language.Lua.Syntax as Lua

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

data ComponentDefinitionError
  = ComponentDefinitionParseError (Lua.SourceRange, String)
  | ComponentDefinitionInterpretError Text
  | ComponentDefinitionTypeError Text
  deriving (Eq, Show)

parseDefinition :: Text -> Either ComponentDefinitionError (Text, Definition)
parseDefinition luaCode = do
  let parsed = Lua.parseText Lua.stat luaCode
  case parsed of
    Right (Lua.Assign [Lua.VarName (Lua.Name label)] [Lua.TableConst fields]) -> do
      fieldMap <- traverse getField fields
      let defMap = Definition $ Map.fromList fieldMap
      pure (label, defMap)
    Right _ -> Left $ ComponentDefinitionInterpretError "Invalid Lua Definition" -- TODO better error msg
    Left parseErr -> Left $ ComponentDefinitionParseError parseErr
  where
    getField :: Lua.TableField -> Either ComponentDefinitionError (Text, DType)
    getField (Lua.ExpField (Lua.String name) (Lua.String val)) = do
      dType <- parseFieldType val
      pure (strip name, dType)
    getField _ = Left $ ComponentDefinitionInterpretError "Invalid Lua field definition" -- TODO better erro msg

    strip :: Text -> Text
    strip = dropAround isPunctuation

parseFieldType :: Text -> Either ComponentDefinitionError DType
parseFieldType "\"int\"" = pure DInt
parseFieldType "\"str\"" = pure DString
parseFieldType "\"num\"" = pure DNum
parseFieldType other = Left . ComponentDefinitionTypeError $ "Unknown component definition type: " <> other
