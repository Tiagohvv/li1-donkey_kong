module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12324

-- | Colisoes entre personagens

p1 = Personagem (0,0) Jogador (4,4) Este (1,1) False False 10 0 (False, 0.0) False
p2 = Personagem (0,0) Fantasma (4,4) Oeste (1,1) True False 2 0 (False, 0.0) False

teste1 = "T1: Personagens colidem " ~: True ~=? colisoesPersonagens p1 p2

p3 = Personagem (0,0) Jogador (2,7) Este (1,1) False False 10 0 (False, 0.0) False
p4 = Personagem (0,0) Fantasma (4,4) Oeste (1,1) True False 2 0 (False, 0.0) False

teste2 = "T2: Personagens nao colidem " ~: False ~=? colisoesPersonagens p3 p4

p5 = Personagem (0,0) Jogador (3,2) Este (1,1) False False 10 0 (False, 0.0) False
p6 = Personagem (0,0) Fantasma (3,3) Oeste (1,1) True False 2 0 (False, 0.0) False

teste3 = "T3: Personagens colidem " ~: True ~=? colisoesPersonagens p5 p6

p7 = Personagem (0,0) Jogador (3,3) Este (1,1) False False 10 0 (False, 0.0) False
p8 = Personagem (0,0) Fantasma (3.1,3) Oeste (1,1) True False 2 0 (False, 0.0) False

teste4 = "T4: Personagens colidem " ~: True ~=? colisoesPersonagens p7 p8
-- | Colisoes com paredes

blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

pl1 = Personagem (0.0,0.0) Jogador (1,1) Este (2,2) False False 10 0 (False, 0.0) False

teste5 = "T5 Jogador nao colide com nenhuma parede " ~: False ~=? colisoesParede gameMap1 pl1

pl2 = Personagem (0.0,0.0) Jogador (0.0,6.5) Este (1,1) False False 10 0 (False, 0.0) False

teste6 = "T6: Jogador colide com limite lateral " ~: True ~=? colisoesParede gameMap1 pl2

pl3 = Personagem (0.0,0.0) Jogador (50.0,0.2) Este (1,1) False False 10 0 (False, 0.0) False

teste7 = "T7: Jogador colide com limite superior " ~: True ~=? colisoesParede gameMap1 pl3

pl4 = Personagem (0.0,0.0) Jogador (3.0,5.0) Este (1,1) False False 10 0 (False, 0.0) False

teste8 = "T8: Jogador colide com plataforma " ~: True ~=? colisoesParede gameMap1 pl4

testesTarefa1 = test [teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8]
