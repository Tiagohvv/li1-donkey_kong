module Auxil where

import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4


{-

sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8)) | (p1 <= p5 && p5 <= p3 || p1 <= p7 && p7<= p3 ) && (p2 <= p6 && p6 <= p4 || p2 <= p8 && p8<= p4 ) = True
                                                 | otherwise = False 

{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>colisao ((1,1),(2,2))  [((1.5,1.5),(3.5,3.5)), ((1.5,1.5),(3.5,3.5))]
True
-}

{-
colisao :: Hitbox -> [Hitbox] -> Bool
colisao _ [] = True
colisao ((p1,p2),(p3,p4)) (((p5,p6),(p7,p8)):t) | sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8)) == False = False
                                                | otherwise = colisaoc ((p1,p2),(p3,p4)) t
-}
{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os Blocos.

== Exemplos

>>> posicaoBlocos (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma]]
[(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
-}
--posicaoBlocos :: Posicao -> [[Bloco]] -> [Posicao]
--posicaoBlocos _ [] = []
--posicaoBlocos (x, y) (h:t) = posicaoBlocop (x, y) h ++ posicaoBlocos (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição desses Blocos.

== Exemplos

>>> posicaoBlocop (0.0,0.0) [Vazio,Escada,Plataforma]
[(1.0,0.0),(2.0,0.0)]
-}
posicaoBlocop :: Posicao -> [Bloco] -> [Posicao]
posicaoBlocop _ [] = []
posicaoBlocop (x, y) (h:t)
    | h == Vazio = posicaoBlocop (x + 1, y) t
    | otherwise = (x, y) : posicaoBlocop (x + 1, y) t


{-| Função que dada uma posição inicial e uma lista de blocos dá a hitbox desses blocos

== Exemplos

>>> 

-}
espacoBloco :: Posicao -> [[Bloco]] -> [Hitbox]
espacoBloco _ [] = []
espacoBloco (x,y) (h:t) = ((posicaoBlocos (x,y) (Bloco Mapa)), (posicaoBlocos (x + 1,y + 1) (Bloco Mapa))) : espacoBloco (x,y) t 


{-| Função que dada a posiçao de uma personagem e o seu tamanho cria a sua hitbox.

== Exemplos

>>> 

-}
criaHitbox :: Posicao -> Personagem -> Hitbox
criaHitbox (x,y) tamanho Personagem = ((x,y), (x + (fst (tamanho Personagem ))), (y + (snd (tamanho Personagem ))))



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

-}