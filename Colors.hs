module Colors where
import Data.List hiding (partition)
import Data.Char

weight :: String -> Int
weight xs = sum [ count x xs | x <- xs ]

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x==y = 1 + count x ys
               | otherwise = count x ys 

sortedColors :: [String]
sortedColors = sortBy cmpWeight colors

cmpWeight :: String -> String -> Ordering
cmpWeight a b = compare (weight a) (weight b)

-- Builds sequences of 4 colors out of 6 possible colors. 
colors :: [String]
colors = 
    mix (chars 6) 6
    where
      chars' = map chr [97..122] 
      chars i = take i chars'

mix :: [a] -> Int -> [[a]]
mix xs 0 = [[]]
mix xs i = 
    [ x:xs' | x <- xs, xs' <- mix xs (i-1) ]
