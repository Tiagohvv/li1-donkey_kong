{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Data.Fixed (E0)

{--colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Bloco Mapa) Posicao Personagem | x == limite || y == limite = True
                                               | posicaoBlocos 
                                               | otherwise = False
                                               



colisoesPersonagens :: Personagem -> [Personagem] -> Bool
colisoesPersonagens | sobreposicao ((personagem posicao), (personagem posicao))  = True 
                    | otherwise = False -}

{-teste
type Hitbox = (Posicao, Posicao)


-- | Posicao no 'Mapa'.
type Posicao = (Double, Double)


-}

sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8))| (p1 <= p5 && p5 <= p3 || p1 <= p7 && p7<= p3 ) && (p2 <= p6 && p6 <= p4 || p2 <= p8 && p8<= p4 ) = True
                                                |otherwise = False 

posicaoBlocos :: (Int, Int) -> [Bloco] -> [Posicao]
posicaoBlocos (x,y) (h:t) = posicaoBlocop (x,y) h : posicaoBlocos (x,y+1) t
                                 where x == 0 && y == 0

posicaoBlocop :: (Int, Int) -> [Bloco] -> [Posicao]
posicaoBlocop (x,y) (h:t) |h == V = posicaoBlocop (x+1,y) t
                          |otherwise = (x,y) : posicaoBlocop (x+1,y) t
                              where x == 0 && y == 0