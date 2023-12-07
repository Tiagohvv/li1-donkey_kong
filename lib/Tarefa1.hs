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
import Tarefa3


colisoesParede :: [[Bloco]] -> Personagem -> Bool
colisoesParede (mapaTeste) per2 | (posicao per2) == (100.0,0.0) || (posicao per2) == (100.0,0.0) = True
                                | colisao (criaHitbox per2) (espacoBloco (posicaoBlocos (0.0,0.0) (mapaTeste))) = True
                                | otherwise = False


colisoesPersonagens :: Personagem -> [Personagem] -> Bool
colisoesPersonagens per2 [per,per3] | colisao (criaHitbox per2) [(criaHitbox (per)), (criaHitbox (per3))] = True 
                                    | otherwise = False


{-teste
type Hitbox = (Posicao, Posicao)


-- | Posicao no 'Mapa'.
type Posicao = (Double, Double)

--}



{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>> sobreposicao ((1,1),(2,2)) ((1.5,1.5),(3.5,3.5))
True
-}

sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8)) | (p1 <= p5 && p5 <= p3 || p1 <= p7 && p7<= p3 ) && (p2 <= p6 && p6 <= p4 || p2 <= p8 && p8<= p4 ) = True
                                                 | otherwise = False 

{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>colisao ((1,1),(2,2))  [((1.5,1.5),(3.5,3.5)), ((1.5,1.5),(3.5,3.5))]
True
-}


colisao :: Hitbox -> [Hitbox] -> Bool
colisao _ [] = True
colisao ((p1,p2),(p3,p4)) (((p5,p6),(p7,p8)):t) | sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8)) == False = False
                                                | otherwise = colisao ((p1,p2),(p3,p4)) t

{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os Blocos.

== Exemplos

>>> posicaoBlocos (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma]]
[(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
-}
posicaoBlocos :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocos _ [] = []
posicaoBlocos (x, y) (h:t) = posicaoBlocop (x, y) h ++ posicaoBlocos (x, y + 10) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição desses Blocos.

== Exemplos

>>> posicaoBlocop (0.0,0.0) [Vazio,Escada,Plataforma]
[(1.0,0.0),(2.0,0.0)]
-}
posicaoBlocop :: Posicao -> [Bloco] -> [Posicao]
posicaoBlocop _ [] = []
posicaoBlocop (x, y) (h:t)
    | h == Vazio = posicaoBlocop (x + 10, y) t
    | otherwise = (x, y) : posicaoBlocop (x + 10, y) t



{-| Função que dada uma posição cria a Hitbox de um Bloco

== Exemplos

>>>espacoBloco [(0.0,0.0),(10.0,10.0),(10.0,0.0),(20.0,10.0)]

[((0.0,0.0),(10.0,10.0)),((10.0,10.0),(20.0,20.0)),((10.0,0.0),(20.0,10.0)),((20.0,10.0),(30.0,20.0))]
-}

espacoBloco :: [Posicao] -> [Hitbox]
espacoBloco [] = []
espacoBloco ((x,y):t) = ((x,y), (x + 10,y + 10)) : espacoBloco t 


{-| Função que dada uma personagem cria a sua hitbox.

== Exemplos

>>> criaHitbox per2
((3.0,4.0),(13.0,24.0))

-}

criaHitbox :: Personagem -> Hitbox
criaHitbox per2 = ((fst (posicao per2),snd (posicao per2)), (fst (posicao per2) + (fst (tamanho per2)), snd (posicao per2) + (snd (tamanho per2))))

