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
colisoesParede (Bloco Mapa) Posicao Personagem | x == limite || y == limite = True
                                               | sobreposicao (posicao personagem) (espacoBloco (0,0) (Bloco Mapa))
                                               | otherwise = False


colisoesPersonagens :: Personagem -> [Personagem] -> Bool
colisoesPersonagens | sobreposicao ((posicao personagem), (posicao inimigos)) = True 
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



{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os Blocos.

== Exemplos

>>> posicaoBlocos (0,0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma]]
[(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
-}
posicaoBlocos :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocos _ [] = []
posicaoBlocos (x, y) (h:t) = posicaoBlocop (x, y) h ++ posicaoBlocos (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição desses Blocos.

== Exemplos

>>> posicaoBlocop (0,0) [Vazio,Escada,Plataforma]
[(1.0,0.0),(2.0,0.0)]
-}
posicaoBlocop :: Posicao -> [Bloco] -> [Posicao]
posicaoBlocop _ [] = []
posicaoBlocop (x, y) (h:t)
    | h == Vazio = posicaoBlocop (x + 1, y) t
    | otherwise = (x, y) : posicaoBlocop (x + 1, y) t


{-| Função que dada uma posição inicial e uma lista de Blocos dá a posição desses Blocos.

== Exemplos

>>> 

-}
espacoBloco :: Posicao -> [[Bloco]] -> [Hitbox]
espacoBloco _ [] = []
espacoBloco (x,y) (h:t) = ((posicaoBlocos (x,y) (Bloco Mapa)), (posicaoBlocos (x + 1,y + 1) (Bloco Mapa))) : espacoBloco (x,y) t 

criaHitbox :: Posicao -> Personagem -> Hitbox
criaHitbox (x,y) tamanho Personagem = ((x,y), (x + (fst (tamanho Personagem ))), (y + (snd (tamanho Personagem ))))









----- validar escadas


validaesc :: Mapa -> Bool
validaesc (Bloco Mapa) | validacoisasal (posicaoBlocosesc) (posicaoBlocosplat) == False
                       | validacoisas (posicaoBlocosesc) (posicaoBlocosplat) == False
                       | otherwise = True  

validacoisas :: [Posicao] -> [Posicao] -> Bool
validacoisasposicaoBlocos _ [] = []
validacoisas ((h,r):t) ((x:z):y) | (h,r) == (x,(z+1)) = False
                                 | otherwise = validacoisas t ((x:z):y)
                                 

validacoisas :: [Posicao] -> [Posicao] -> Bool
posicaoBlocos _ [] = []
validacoisas ((h,r):t) ((x:z):y) | (h,r) == (x,z) = False
                                 | (h,r) /= (x,z)  = validacoisas t ((x:z):y)
                                 | otherwise = True

validacoisasal :: [Posicao] -> [Posicao] -> Bool
posicaoBlocos _ [] = []
validacoisasal ((h,r):t) ((x:z):y) | (h,r) == (x,z) = False
                                 | (h,r) /= (x,z)  = validacoisasal t ((x:z):y)
                                 | otherwise = True 

posicaoBlocosesc :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocos _ [] = []
posicaoBlocosesc (x, y) (h:t) = posicaoesc (x, y) h ++ posicaoesc (x, y + 1) t


posicaoesc :: Posicao -> [Bloco] -> [Posicao]
posicaoesc _ [] = []
posicaoesc (x, y) (h:t)
    | h == Escada = (x, y) : posicaoesc (x + 1, y) t
    | otherwise = posicaoesc (x + 1, y) t


posicaoBlocosplat :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosplat _ [] = []
posicaoBlocosplat (x, y) (h:t) = posicaoplat (x, y) h ++ posicaoplat (x, y + 1) t


posicaoplat :: Posicao -> [Bloco] -> [Posicao]
posicaoplat _ [] = []
posicaoplat (x, y) (h:t)
    | h == Plataforma = (x, y) : posicaoplat (x + 1, y) t
    | otherwise = posicaoplat (x + 1, y) t



posicaoBlocosal :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosal _ [] = []
posicaoBlocosal (x, y) (h:t) = posicaoal (x, y) h ++ posicaoal (x, y + 1) t


posicaoal :: Posicao -> [Bloco] -> [Posicao]
posicaoal _ [] = []
posicaoal (x, y) (h:t)
    | h == Alcapao = (x, y) : posicaoal (x + 1, y) t
    | otherwise = posicaoal (x + 1, y) t

validacoisas :: [Double] -> [Double] -> Bool
validacoisas (h:t) (x:y) | h == x = False
                         | h /= x = validacoisas t (x:y)
                         | otherwise = True  

