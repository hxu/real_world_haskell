safeHead :: [a] -> Maybe a

safeHead (x:xs) = Just x
safeHead [] = Nothing

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p (x:xs) | not (p x) = splitWith p xs
splitWith p xs = [left] ++ (splitWith p right)
    where (left, right) = span p xs

myTranspose :: String -> String
myTranspose [] = []
myTranspose input | input == "" = []
                    | otherwise= tpose splitInput ++ "\n" ++ myTranspose (unlines (map tail splitInput))
            where splitInput = lines input
                  tpose ls = map head ls

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x = x : (myTakeWhile p xs)
    | otherwise = []

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 _ [] = []
myTakeWhile2 p xs = foldr step [] xs
    where step x ys | p x = x : ys | otherwise = []

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _ [] = []
myGroupBy p xs = foldr step [] xs
    where step x (y:ys) | p x y = x 
                        | otherwise = []
