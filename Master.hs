module Master where
import Data.List
import Colors

--ABC
check :: String -> String -> (Int,Int)
check code guess = 
    (i, j-i)
      where 
       i = checkExact code guess -- Exact match (black pegs)
       j = checkNear code guess  -- Near match  (white pegs)

-- black pegs
checkExact :: String -> String -> Int
checkExact [] [] = 0
checkExact (x:xs) (y:ys) | x==y = 1 + checkExact xs ys
                         | otherwise = checkExact xs ys 

-- white pegs
checkNear :: String -> String -> Int
checkNear [] _ = 0
checkNear (x:xs) ys = 
    checkNear' x id ys
        where
         checkNear' _ f [] = checkNear xs (f [])
         checkNear' x f (y:ys) | x==y = 1 + checkNear xs (f ys)
                               | otherwise = checkNear' x (f.(y:)) ys 

-- part "abcde" 1
-- ["a","b","c","d","e"]
-- part "abcde" 3
-- ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
part :: [a] -> Int -> [[a]]  
part xs = ([ part' xs j | j <- [0..] ] !!)
    where
    part' _ 0 = [[]]
    part' [] _ = []
    part' (x:xs) i = 
        let 
         f = part xs
        in [ x:xs' | xs' <-  f (i-1) ] ++ f i

data F = At (Char,Int)
       | In Char
       | And [F]
       | Or [F]
       | Not F
       deriving Eq

instance Show F where
 show (At (x,i)) = x:show i
 show (In x) = x:[]
 show (Not x) = '-':show x
 show (And xs) = '^': show xs
 show (Or xs)  = 'v': show xs

positions xs = zip xs [0..]

-- Ex: v[^[b0,-a],^[-e2,f]] 
-- Read: b is at 0 and a is not in the code, or e is not at 2 and f is in the code
frm :: String -> (Int,Int) -> F
frm xs (i,j) =
    let 
     xs0 = positions xs -- labeled chars
     xs1 = part xs0 i -- exact partition
     as0 = [ (x1,map fst (xs0\\x1)) | x1 <- xs1 ] -- (exact, difference over input)
     as1 = [ (x1,part a0 j) | (x1,a0) <- as0 ] -- (exact, near partitions)
     xs' = nub xs
    in case frm' xs' xs xs0 as1 of 
        [x] -> x
        xs -> Or xs

frm' _ _ ys [] = []
frm' zs xs ys ((exact,near):xss) =
    let
     es = frmExact (ys\\exact) exact
     ns = frmNear (zs\\es') near
     es' = map fst exact
    in case frmOr es ns of 
     [v] -> v: frm' zs xs ys xss
     vs -> Or vs: frm' zs xs ys xss

frmExact xs as = frmExact' as ++ frmNotAt xs
    where
    frmExact' [] = []
    frmExact' (a:as) = At a: frmExact' as
    frmNotAt [] = []
    frmNotAt (a:as) = Not (At a): frmNotAt as

frmNear xs [] = []
frmNear xs (as:ass) = (map (In) as ++ frmNotIn as xs): frmNear xs ass
    where 
    frmNotIn xs [] = []
    frmNotIn xs (b:bs) | b `notElem` xs = Not (In b):frmNotIn xs bs
                       | otherwise = frmNotIn xs bs

frmOr _ [] = []
frmOr as (bs:bss) = And (as++bs): frmOr as bss

-- Evaluates a formula 
eval :: String -> F -> Bool
eval xs (At (x,i)) = xs!!i==x
eval xs (In x) = x `elem` xs
eval xs (Not x) = not (eval xs x)
eval xs (And ys) = all (eval xs) ys
eval xs (Or ys) = any (eval xs) ys 


