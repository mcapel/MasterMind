# MasterMind
This Haskell program is a code breaker for the popular game of MasterMind https://en.wikipedia.org/wiki/Mastermind_(board_game)

Pegs are represended with letters 'a' to 'f'.
You can run the Test.hs file and try: 
test "ccba" 
(where "ccba" is the code)

The computer will start guessing. It checks automatically the white and black pegs, and on this basis it builds a sort of propositional formula constraining the space of possible further guesses. On average the program needs 3.8 guesses before finding the right one. The procedure can be generalized on more colors and longer sequences (Change the Colors file to generalize).

There is also a program in which the computer gives a code, and you need to break it, Play.hs. I use it to see who's best between me and the computer.... I stand no chance.
