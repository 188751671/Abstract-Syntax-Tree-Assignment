module Game where

import Data.Either
import Data.Char (toLower)
import JavaScript

-- # Map items
data KeyColor = Orange | Yellow | Blue
  deriving Show

data MapItem =
    Home
    -- ^ Home square (end of game if player goes over it)
  | Rock Integer
  -- ^ Rocks (numbered 0 to 4 inclusive)
  | TreeA
  -- ^ Tree A
  | TreeB
  -- ^ Tree B
  | None
  -- ^ Empty (grass) square
  | Door { target :: RoomId, requirements :: [KeyColor] }
  -- ^ Doors, which include the name of a room which it leads to
  -- and a list of keys that are needed top open the door.
  deriving Show

-- Representation of a room in the game,
-- a list of list of either `MapItem` or the type parameter
-- which will represent action items.
type Room items = [[Either MapItem items]]      -- data Either a b = Left a | Right b
-- Rooms just have string names
type RoomId = String

-- Configuration of the game
data GameConfig items = GameConfig {
    rooms         :: [(RoomId, Room items)],               
    -- ^ association list from room names to room configs

    actions       :: (items -> JSExpr),
    -- ^ a function mapping action items to the JavaScript code
    -- saying what happens when Lambda goes over that item

    actionItems :: [items]
    -- ^ a list of actionable items we want to allow in this
    -- game (should be everything we want `actions` to handle)
  }

-- The main function we will use, given a game config, and a function
-- to generate CSS class names for an item, write it to config.hs
outputConfig :: GameConfig items -> (items -> String) -> IO ()
outputConfig cfg itemName =
  writeFile "config.js" (compileBlock $ compileConfig itemName cfg)

---------------------
-- What follows are the rest of the functions used by `outputConfig`

-- Gives the class name used in the HTML to represent this map item
mapItemToName :: MapItem -> String
mapItemToName Home     = "home"
mapItemToName (Rock n) = "rock"  ++ show n
mapItemToName TreeA    = "treea"
mapItemToName TreeB    = "treeb"
mapItemToName None     = "none"
mapItemToName (Door _ _) = "door"

-- Compiles the map item to its internal representation in the JavaScript
mapItemCompile :: MapItem -> JSExpr
mapItemCompile (Door target reqs)  =
  Object [("fst", JString "door")
        , ("snd", JString target)
        , ("reqs", List $ map (JString . show) reqs)]
mapItemCompile t = jsPair (JString $ mapItemToName t) Null

-- Compilees a room to its JS representation
compileRoom :: (items -> String) -> Room items -> JSExpr
compileRoom itemName items =
  List (map (List . map (either mapItemCompile leftPair)) items)
    where
      leftPair x = jsPair (JString $ itemName x) Null

-- Compiles the entire board to its JS representation
compileRooms :: (items -> String) -> [(RoomId, Room items)] -> JSExpr
compileRooms itemName rooms =
  Object (map (\(n, room) -> (n, compileRoom itemName room)) rooms)

-- Compiles a game config into a JSBlock
compileConfig :: (items -> String) -> GameConfig items -> JSBlock
compileConfig itemName (GameConfig rooms actions actionItems) =
  case lookup "start" rooms of
    Nothing -> error "A game config must have at least one board named \"start\""
    Just _ ->
      Next (Decl "actions" (Object (map (\i -> (itemName i, actions i)) actionItems)))
    $ Next (Decl "gameMap" (compileRooms itemName rooms))
    $ Next (Decl "objects" (Property "gameMap" "start")) Nil

--------------------

