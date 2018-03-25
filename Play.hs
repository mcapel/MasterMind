module Play where
import System.Random
import Data.Char
import Master
import Colors

play =
 do 
  code <- rnd_select colors
  try' code

try code = 
 do
  guess <- getLine 
  let 
   (i,j) = check code guess
  print (i,j)
  if i==4 then print ("success " ++ guess)
          else try code

try' code = 
 do
  guess <- getLine 
  let 
   out = check code guess
   f = frm guess out
  print out
  print (show f)
  if (fst out) ==4 
  then print ("success " ++ guess)
  else try' code

rnd_select :: [a] -> IO a
rnd_select [] = error "empty list"
rnd_select l  = do 
                 p <- getStdRandom $ randomR (0, (length l)-1)
                 return (l!!p)












