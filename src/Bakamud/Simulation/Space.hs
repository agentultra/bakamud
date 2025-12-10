{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bakamud.Simulation.Space where

import Control.Error.Util
import Control.Monad (join)
import Data.Char (isPunctuation)
import Data.Text (Text, dropAround)
import qualified Data.Map.Strict as Map
import qualified Language.Lua.Annotated.Lexer as Lua
import qualified Language.Lua.Parser as Lua
import qualified Language.Lua.Syntax as Lua

data Exit
  = Exit
  { exitName :: Text
  , goesTo   :: Text
  }
  deriving (Eq, Show)

data Room
  = Room
  { roomName        :: Text
  , roomDescription :: Text
  , roomExits       :: [Text] -- TODO: make this better... maybe more constraints on valid exit lables in Room?
  }
  deriving (Eq, Show)

data RoomFieldType
  = RoomText Text
  | RoomListText [Text]
  deriving (Eq, Ord, Show)

data RoomDefinitionError
  = RoomDefinitionParseError (Lua.SourceRange, String)
  | RoomDefinitionInterpretError Text
  | RoomDefinitionFieldError Text
  deriving (Eq, Show)

parseRoomDefinition :: Text -> Either RoomDefinitionError (Text, Room)
parseRoomDefinition luaCode = do
  let parsed = Lua.parseText Lua.stat luaCode
  case parsed of
    Left parseError -> Left $ RoomDefinitionParseError parseError
    Right (Lua.Assign [Lua.VarName (Lua.Name label)] [Lua.TableConst fields]) -> do
      fieldMap <- Map.fromList <$> traverse getField fields
      roomName <- note (RoomDefinitionFieldError "Missing name")
        $ join
        $ getRoomFieldText <$> Map.lookup "name" fieldMap
      roomDescription <- note (RoomDefinitionFieldError "Missing description")
        $ join
        $ getRoomFieldText <$> Map.lookup "description" fieldMap
      roomExits <- note (RoomDefinitionFieldError "Missing exits")
        $ join
        $ getRoomExits <$> Map.lookup "exits" fieldMap
      pure (label, Room {..})
    Right _ -> Left $ RoomDefinitionInterpretError "Invalid room definition"
  where
    getField :: Lua.TableField -> Either RoomDefinitionError (Text, RoomFieldType)
    getField (Lua.ExpField (Lua.String name) (Lua.String val)) = do
      pure (strip name, RoomText val)
    getField (Lua.ExpField (Lua.String name) (Lua.TableConst fields)) = do
      exits <- traverse getExitField fields
      pure (strip name, RoomListText exits)
    getField _ = Left $ RoomDefinitionInterpretError "Invalid room definition" -- TODO better erro msg

    getExitField :: Lua.TableField -> Either RoomDefinitionError Text
    getExitField (Lua.Field (Lua.PrefixExp (Lua.PEVar (Lua.VarName (Lua.Name name))))) =
      pure name
    getExitField _ = Left $ RoomDefinitionFieldError "Not valid exit variable"

    getRoomFieldText :: RoomFieldType -> Maybe Text
    getRoomFieldText (RoomText txt) = pure txt
    getRoomFieldText _              = Nothing

    getRoomExits :: RoomFieldType -> Maybe [Text]
    getRoomExits (RoomListText exits) = pure exits
    getRoomExits _                    = Nothing

    strip :: Text -> Text
    strip = dropAround isPunctuation
