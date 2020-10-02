module Dados where 

--  vazio 1 (peça 1 vazia) player x
data CASA = Jogador Char
          | Vazio Int 
         deriving (Read,Eq)

type TAB = [CASA]

t1::TAB
t1 = [Vazio 1, Vazio 2, Vazio 3, Vazio 4, Vazio 5 , Vazio 6, Vazio 7, Vazio 8, Vazio 9]

-- Definir show para os novos tipos definidos em dados
instance Show CASA where
    show (Vazio n) = show n
    show (Jogador c) = [c]





