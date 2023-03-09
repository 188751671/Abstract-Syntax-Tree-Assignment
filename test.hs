import Game
import JavaScript

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
                            _   -> Left None


string_To_ListOfItems :: String -> [Either MapItem a] -> [Either MapItem a]
string_To_ListOfItems string accumulator = foldr (\char acc -> charToMapItemValue char : acc) accumulator string


main :: IO ()
main = do
    print ""




{-}

-}
