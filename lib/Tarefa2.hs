{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

valida :: Jogo -> Bool
valida jogo = undefined

--validaressalta :: Personagem -> [Personagem] -> Bool 
--validaressalta jog = not (ressalta jog) &&  

validachao :: Mapa -> Bool 
validachao (Mapa _ _ matriz) = all (==Plataforma) (last matriz)




