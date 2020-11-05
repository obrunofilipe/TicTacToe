module Grafica where
import Dados
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color


--board to string 
showTab :: TAB -> String
showTab tab = line1 ++ newLine ++ line2 ++ newLine ++ line3
            where (l1,l2,l3) = makeLines tab 
                  newLine    = "---------\n"
                  line1      = printLine l1
                  line2      = printLine l2
                  line3      = printLine l3

-- converts a line into a string 
printLine :: TAB -> String
printLine [h]   = (show h) ++ "\n"
printLine (h:t) = (show h) ++ " | " ++ (printLine t)

-- breaks the board into lines 
makeLines :: [a] -> ([a],[a],[a])
makeLines tab = (line1, line2, line3)
              where (newtab,line3) = splitAt 6 tab
                    (line1,line2)  = splitAt 3 newtab

window :: Display
window = InWindow "TicTacToe" (800,800) (800,800)

c1 :: Color
c1 = black 

bk :: Picture
bk = Blank

piece :: Picture
piece = color red (Polygon [(0,0),(266.7,0),(266.7,266.7),(0,266.7)])


