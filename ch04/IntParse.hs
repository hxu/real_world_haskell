import Data.Char (digitToInt)
import Data.List

asInt :: String -> Int
asInt xs = loop 0 xs
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                      in loop acc' xs

asInt_fold :: String -> Int
asInt_fold [] = error "Empty string provided"
asInt_fold "-" = error "Need more than just a dash"
asInt_fold xs
    | isInfixOf "." xs = error "Can't handle decimals"
    | head xs == '-' = -1 * asInt_fold (tail xs)
    | otherwise = foldl buildInt 0 xs
        where buildInt acc x = (acc * 10) + digitToInt x

