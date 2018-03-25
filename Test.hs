module Test where 
import Master
import Colors

-- test "aacc"
test x = 
    do
     print ("Code: " ++ x)
     play 0 x sortedColors
         

play i y [] = print ("Fail after " ++ show i)
play k y (x:xs) = 
            do 
             print ("Guess: " ++ x)
             print ("Worlds:")
             print (length xs)
             let (i,j) = check y x
             if i == length y 
             then print ("Success after " ++ show k)
             else do 
                   let ps' = frm x (i,j)
                       xs' = filter (\ x -> eval x ps') xs
                   print ("Outcome: " ++ show (i,j))
                   print ("Val: " ++ show ps')
                   play (k+1) y xs'

