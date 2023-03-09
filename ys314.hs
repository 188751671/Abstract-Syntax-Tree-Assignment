{-# OPTIONS_GHC -Wno-typed-holes #-}
import Game
import JavaScript



{-
    1.Simple Map Generation
-}


generate_EmptyRoom :: Int -> Room items
generate_EmptyRoom n
            | n <= 0     = []
            | otherwise  = replicate n (replicate n (Left None))


nullString :: items -> JSExpr
nullString x = Null

start :: Room items
start = generate_EmptyRoom 10


initGameConfig :: GameConfig items
initGameConfig = GameConfig {
    rooms = [("start", start)],
    actions = nullString,
    actionItems = []
}


q1 :: IO ()
q1 = outputConfig initGameConfig mapItemToName



{-
    2.Map Builder
-}


-- ( a )  using double Map to convert
mapBuilder :: (Char -> Either MapItem a) -> [String] -> Room a  -- [[Either MapItem items]]
mapBuilder func []      = [[]]
mapBuilder func strings = map (\str -> map charToMapItemValue str) strings



-- ( b )
charToMapItemValue :: Char -> Either MapItem a
charToMapItemValue char = case char of 
                            'A' -> Left TreeA    -- Triangular tree
                            'B' -> Left TreeB    -- Bushy tree
                            'H' -> Left Home
                            '-' -> Left None
                            '0' -> Left (Rock 0)   -- smallest rock
                            '1' -> Left (Rock 1)   -- medium rock towards right
                            '2' -> Left (Rock 2)   -- medium rock towards left
                            '3' -> Left (Rock 3)   -- largest rock
                            '4' -> Left (Rock 4)
                            _   -> error "Invalid map item character"



--  ( c )
dilatingMap :: a -> [a] -> [a]
dilatingMap a []     = []           -- base case: empty list
dilatingMap a [x]    =  x:a:[]      -- base case: list with one element
dilatingMap a (x:xs) = x:a:dilatingMap a xs -- recursive case


-- insert input arg after each element, then insert empty row of the same length after each row
dilate :: Room a -> Room a
dilate room = dilatingMap emptyRow newList
    where
        newList = [dilatingMap (Left None) list | list <- room]   -- a new List after inserting empty cell after each element
        emptyRow  = replicate (length (head newList)) (Left None)


--  ( d )
mapStrings :: [String]
mapStrings = ["HA-B2-","-132BA","1A-2A3","B0-12-","-212-0","1-31BA"]

gameConfig :: GameConfig items
gameConfig = GameConfig {
    rooms = [("start",dilate (mapBuilder charToMapItemValue mapStrings))],
    actions = nullString,
    actionItems = []
}


q2 = outputConfig gameConfig mapItemToName



{-
    3.Action Items
-}


-- ( a )
data ActionItem =  ChocolateSnack | Coin KeyColor
    deriving Show

-- ( b )
actionItemName :: ActionItem -> String
actionItemName ChocolateSnack   = "food"
actionItemName (Coin Yellow)    = "coingold"
actionItemName (Coin Orange)    = "coincopper"


-- ( c )
charToMapItemOrActionItem :: Char -> Either MapItem ActionItem
charToMapItemOrActionItem 'C' = Right ChocolateSnack
charToMapItemOrActionItem 'Y' = Right (Coin Yellow)
charToMapItemOrActionItem 'O' = Right (Coin Orange)
charToMapItemOrActionItem  k  = Left (eitherToMapItem(charToMapItemValue k))

eitherToMapItem :: Either MapItem a -> MapItem
eitherToMapItem (Left mi) = mi
eitherToMapItem (Right _) = None


-- ( d )
jsId :: JSExpr
jsId = Function ["a"] (Return (Var "a")) 


-- ( e )

updatePlayerData :: [(String, JSExpr -> JSExpr)] -> JSExpr

updatePlayerData newData =
    Function ["data"] $ Return $
        Object [("energy", (changes "energy" newData)), 
            ("score", (changes "score" newData)), 
            ("inventory", (changes "inventory" newData))]
        where
            changes theProperty newData =
                case lookup theProperty newData of
                    Just newData -> newData (Property "data" theProperty)
                    Nothing -> Property "data" theProperty
















{-
-- ("score" , \x -> BinOp "+" x (Num 1))
-- score: pdata.score + 1
-- ("score", BinOp + (Property "pdata" "score") Num 1 )
converter :: (String, JSExpr -> JSExpr) -> JSExpr
converter x = case head x of
                "energy"    -> (tail x) (Property "pdata" "energy")
                "score"     -> (tail x) (Property "pdata" "score")
                "inventory" -> (tail x) (Property "pdata" "inventory")

data Items = Energy | Score | Inventory



updatePlayerData [  ("score" , \x -> BinOp "+" x (Num 1))  ]

Function ["pdata"] (Return  (Object [  ((head x),(last x))  ])  )

function (pdata) {
return {energy: pdata.energy, score: pdata.score + 1, inventory: pdata.inventory}
}

{fst: 42, snd: 10}
[("fst", Num 42), ("snd", Num 10)]

> let list = [("a", 1), ("b", 2), ("c", 3)]
> lookup "b" list
Just 2
> lookup "d" list
Nothing


function   (x, y)         { block }
Function   ["x", "y"]     block
-}