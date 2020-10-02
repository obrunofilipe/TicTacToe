module Jogo where
import Dados
import Grafica

--definir a função que parte a lista(tabuleiro ) em duas e remove uma peça
removePiece:: [a] -> Int -> ([a],[a])
removePiece lst n = (left,right)
                  where (left,ys) = splitAt (n - 1) lst
                        right = drop 1 ys
  
-- coloca a peça no tabuleiro
addPiece:: [a] -> a -> Int -> [a]
addPiece tab p index = left ++ [p] ++ right
                     where (left,right) = removePiece tab index

--
isDigit :: Int -> Bool 
isDigit n = elem n [1..9]

--check for valid input 
checkInput:: TAB -> IO Int
checkInput tab = do 
                   input <- getChar
                   if isDigit(read[input]) && checkPlace tab (read[input])
                       then return (read [input])
                       else do 
                           putStrLn "Enter a valid position (1-9):"
                           checkInput tab 

--checks if place is available 
checkPlace::TAB -> Int -> Bool
checkPlace [] _                   = False 
checkPlace ((Vazio n):t) index    | n == index = True
                                  | otherwise  = checkPlace t index
checkPlace (h:t) index            = checkPlace t index 

-- checks if there is a winner 
checkWin:: TAB -> Bool
checkWin tab = (a == b && b == c) || (a1 == b1 && b1 == c1) || (a2 == b2 && b2 == c2) || (a == b1 && b1 == c2) || (a2 == b1 && b1 == c) || 
               (a == a1 && a1 == a2) || (b == b1 && b1 == b2) || (c == c1 && c1 == c2)
          where (a:a1:[a2], b:b1:[b2], c:c1:[c2]) = makeLines tab 

--
switchPlayer :: Char -> Char
switchPlayer c | c == 'O' = 'X'
               | c == 'X' = 'O'

-- funçao para rodar o jogo
runTicTacToe :: [CASA] -> Char -> IO()
runTicTacToe tab c = do  
                       putStrLn  (showTab tab)
                       putStrLn "Enter a valid position (1-9):"
                       input <- checkInput tab
                       putStrLn "\n"
                       let newTab = addPiece tab (Jogador c) input
                       if (checkWin newTab) then putStrLn ("Player " ++ [c] ++ " won!")
                                            else runTicTacToe newTab (switchPlayer c)



main :: IO()
main = runTicTacToe tab 'X'
      where tab = [Vazio 1, Vazio 2, Vazio 3, Vazio 4, Vazio 5, Vazio 6, Vazio 7, Vazio 8, Vazio 9] 