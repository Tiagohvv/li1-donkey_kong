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



colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Bloco Mapa) posicao Personagem | x == 100 || y == 100 = True
                                               | sobreposicao (posicao personagem) (espacoBloco (0,0) (Bloco Mapa)) = True
                                               | otherwise = False


colisoesPersonagens :: Personagem -> [Personagem] -> Bool
colisoesPersonagens | colisao ((posicao personagem), (posicao inimigos)) = True 
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
                                                | otherwise = colisaoc ((p1,p2),(p3,p4)) t

{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os Blocos.

== Exemplos

>>> posicaoBlocos (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma]]
[(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
-}
posicaoBlocos :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocos _ [] = []
posicaoBlocos (x, y) (h:t) = posicaoBlocop (x, y) h ++ posicaoBlocos (x, y + 1) t



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


