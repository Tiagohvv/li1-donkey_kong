{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where
import Tarefa1
import LI12324

data Jogo =
  Jogo
    { mapa          :: Mapa -- ^ mapa do jogo
    , inimigos      :: [Personagem] -- ^ lista de inimigos no mapa
    , colecionaveis :: [(Colecionavel, Posicao)] -- ^ lista de colecionaveis espalhados pelo mapa
    , jogador       :: Personagem -- ^ o jogador
    }
  deriving (Eq, Read, Show)

valida :: Jogo -> Bool
valida jogo    | ressalta inimigos == True &&  ressalata jogador == False = False 
               | posicao jogador == posicao inimigos = False
               | validaesc == False = False
               | vida inimigos \= 1 = False
               | colisoesParede == False = False
               |= False
               |= False
               | otherwise = True

--validaressalta :: Personagem -> [Personagem] -> Bool 
--validaressalta jog = not (ressalta jog) &&  

validachao :: Mapa -> Bool 
validachao (Mapa _ _ matriz) = all (==Plataforma) (last matriz)


emBloco :: [Posicao] -> [Posicao] -> Bool


----- validar escadas(por acabar)


{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todas as escadas.

== Exemplos

>>>
-}


validaesc :: Mapa -> Bool
validaesc (Bloco Mapa) | validacoisasal (posicaoBlocosesc) (posicaoBlocosal) == False = False
                       | validacoisasplat (posicaoBlocosesc) (posicaoBlocosplat) == False = False
                       | otherwise = True  


                                 

validaCoisasPlat :: [Posicao] -> [Posicao] -> Bool
validaCoisasPlat [] [] = True
validaCoisasPlat [] _ = True
validaCoisasPlat _ [] = False
validaCoisasPlat ((h,r):t) ((x,z):y) | (h,r) == (x,z+1) || (h,r-1) == (x,z) = validaCoisasPlat t ((x,z):y)
                                     |otherwise = validaCoisasPlat ((h,r):t) y


validaCoisasAl :: [Posicao] -> [Posicao] -> Bool
validaCoisasAl [] [] = True
validaCoisasAl [] _ = True
validaCoisasAl _ [] = False
validaCoisasAl ((h,r):t) ((x,z):y) | (h,r) /= (x,z+1) && (h,r-1) /= (x,z) = validaCoisasAl t ((x,z):y)
                                   | otherwise = False



-----Feito

{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todas as escadas.

== Exemplos

>>> posicaoBlocosesc (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma,Alcapao]]
[(1.0,0.0),(1.0,1.0)]
-}
posicaoBlocosesc :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosesc _ [] = []
posicaoBlocosesc (x, y) (h:t) = posicaoesc (x, y) h ++ posicaoBlocosesc (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição das escadas.

== Exemplos

>>>posicaoal (0.0,0.0) [Vazio,Escada,Plataforma,Alcapao]
[(3.0,0.0)] 
-}
posicaoesc :: Posicao -> [Bloco] -> [Posicao]
posicaoesc _ [] = []
posicaoesc (x, y) (h:t)
    | h == Escada = (x, y) : posicaoesc (x + 1, y) t
    | otherwise = posicaoesc (x + 1, y) t 








{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os alçapoes.

== Exemplos

>>> posicaoBlocosal (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma,Alcapao]]
[(3.0,1.0)]
-}
posicaoBlocosal :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosal _ [] = []
posicaoBlocosal (x, y) (h:t) = posicaoal (x, y) h ++ posicaoBlocosal (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição dos alçapoes.

== Exemplos

>>> posicaoal (0.0,0.0) [Vazio,Escada,Plataforma,Alcapao]
[(3.0,0.0)]
-}
posicaoal :: Posicao -> [Bloco] -> [Posicao]
posicaoal _ [] = []
posicaoal (x, y) (h:t)
    | h == Alcapao = (x, y) : posicaoal (x + 1, y) t
    | otherwise = posicaoal (x + 1, y) t 









{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todas as plataformas.

== Exemplos

>>> posicaoBlocospl (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma,Alcapao]]
[(2.0,0.0),(2.0,1.0)]
-}
posicaoBlocospl :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocospl _ [] = []
posicaoBlocospl (x, y) (h:t) = posicaopl (x, y) h ++ posicaoBlocospl (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição das plataformas.

== Exemplos

>>> posicaopl (0.0,0.0) [Vazio,Escada,Plataforma,Alcapao]
[(2.0,0.0)]
-}
posicaopl :: Posicao -> [Bloco] -> [Posicao]
posicaopl _ [] = []
posicaopl (x, y) (h:t)
    | h == Plataforma = (x, y) : posicaopl (x + 1, y) t
    | otherwise = posicaopl (x + 1, y) t 

