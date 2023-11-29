{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
{-}
data Jogo =
  Jogo
    { mapa          :: Mapa -- ^ mapa do jogo
    , inimigos      :: [Personagem] -- ^ lista de inimigos no mapa
    , colecionaveis :: [(Colecionavel, Posicao)] -- ^ lista de colecionaveis espalhados pelo mapa
    , jogador       :: Personagem -- ^ o jogador
    }
  deriving (Eq, Read, Show)

valida :: Jogo -> Bool
<<<<<<< HEAD
valida jogo    | inimigos ressalta == True && jogador ressalata == False = False 
               | jogador posicao == inimigos posicao = False
               | = False
               | inimigos vida \= 1 = False
               |= False
               |= False
               |= False
               | otherwise = True
-}

               
               
=======
valida jogo = undefined

--validaressalta :: Personagem -> [Personagem] -> Bool 
--validaressalta jog = not (ressalta jog) &&  

validachao :: Mapa -> Bool 
validachao (Mapa _ _ matriz) = all (==Plataforma) (last matriz)




>>>>>>> 7c680f362ebaa823421dca5a9277366c5603c4e4
