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
import Tarefa3 
import GHC.OldList (elemIndices, elemIndex)


valida :: Jogo -> Bool
valida (Jogo mapa inimigos colecionaveis jogador) | validachao mapa == False = False
                                                  | ressalta jogador == False || validaressalta inimigos == False = False 
                                                  | validaposicaoJogador jogador inimigos == False = False
                                                  | containimigos inimigos < 2 = False 
                                                  | validavidas inimigos == False = False
                                                  | validaesc mapa == False = False
                                                  | fst (tamanho jogador) <= 10 = False
                                                  | otherwise = True






validaposicaoJogador :: Personagem -> [Personagem] -> Bool
validaposicaoJogador x [] = True
validaposicaoJogador x (h:t) | colisoesPersonagens x h == True = False
                             | otherwise = validaposicaoJogador x t




containimigos :: [Personagem] -> Int
containimigos [] = 0
containimigos (h:t) = 1 + containimigos t


{-| Função que dada uma lista de personagens verifica se estas têm a propriedade vida igual a 1 
== Exemplos

>>>
-}

validavidas :: [Personagem] -> Bool
validaviads [] = True
validavidas (h:t) | vida h /= 1 = False                                                   
                  | otherwise = validavidas t


{-| Função que dada uma lista de personagens verifica se estas têm a propriedade ressalta a false
== Exemplos

>>>
-}

validaressalta :: [Personagem] -> Bool
validaressalta [] = True
validaressalta (h:t) | ressalta h == False = False                                                   
                     | otherwise = validavidas t



validachao :: Mapa -> Bool 
validachao (Mapa _ _ matriz) = all (==Plataforma) (last matriz)


{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todas as escadas.

== Exemplos

>>>
-}


validaesc :: Mapa -> Bool
validaesc (Mapa _ _ x)  | validaCoisasAl (posicaoBlocosesc (0,0) x) (posicaoBlocosal (0,0) x) == False = False
                        | escadinhas (validaCoisasPlat (posicaoBlocosesc (0.0,0.0) (x)) (posicaoBlocospl (0.0,0.0) (x))) (posicaoBlocosesc (0.0,0.0) (x)) == False = False
                        | otherwise = True  



{-| Função que dada a posição de escadas verifica se estas começam ou acabam escadas

== Exemplos

>>> 
-}


escadinhas :: [Posicao] -> [Posicao] -> Bool
escadinhas _ [] = False
escadinhas [] _ = True
escadinhas ((h,r):t) y | escadinhasaux (h,r) y == True = escadinhas t y
                       | otherwise = False


{-| Função que dada a posição de uma escada verifica se esta começa ou acaba em outra escada

== Exemplos

>>> 
-}


escadinhasaux :: Posicao -> [Posicao] -> Bool
escadinhasaux _ [] = False
escadinhasaux (h,r) ((x,z):y) | (h,r) == (x,z+1) || (h,r) == (x,z-1) = True
                              | otherwise = escadinhasaux (h,r) y



{-| Função que dada a posição de escadas verifica se estas começam ou acabam em plataformas, aplicando a função validaCoisasPlataux, o que faz com que retorne a lista de posições que não cumprem os requesitos dseta função

== Exemplos

>>>
-}

validaCoisasPlat :: [Posicao] -> [Posicao] -> [Posicao]
validaCoisasPlat [] _ = []
validaCoisasPlat ((h,r):t) y = validaCoisasPlataux (h,r) y ++ validaCoisasPlat t y


{-| Função que dada a posição de uma escada verifica se esta começa ou acaba numa em Plataforma, devolvendo a posição de todas as escadas que não começam nem acabam em plataformas

== Exemplos

>>> 
-}


validaCoisasPlataux :: Posicao -> [Posicao] -> [Posicao]
validaCoisasPlataux (h,r) [] = [(h,r)]
validaCoisasPlataux (h,r) ((x,z):y) | (h,r) == (x,z+1) || (h,r) == (x,z-1) = []
                                    | otherwise = validaCoisasPlataux (h,r) y


{-| Função que dada a posição de todas as escadas verifica se estas começam ou acabam em alçapões

== Exemplos

>>> 
-}

validaCoisasAl :: [Posicao] -> [Posicao] -> Bool
validaCoisasAl [] [] = True
validaCoisasAl [] _ = True
validaCoisasAl ((h,r):t) y | validaCoisasAlaux (h,r) y == True = validaCoisasAl t y
                           | otherwise = False


{-| Função que dada a posição de uma escada verifica se esta começa ou acaba num Alçapão

== Exemplos

>>> 
-}


validaCoisasAlaux :: Posicao -> [Posicao] -> Bool
validaCoisasAlaux _ [] = True
validaCoisasAlaux (h,r) ((x,z):y) | (h,r) == (x,z+1) || (h,r) == (x,z-1) = False
                                  | otherwise = validaCoisasAlaux (h,r) y




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






posicaoBlocos :: Bloco -> [[Bloco]] -> [Posicao] 
posicaoBlocos bloco matriz = [(fromIntegral linhas, fromIntegral colunas) | 
                                   colunas <- [0 .. (length matriz)-1]  
                                 , linhas  <- elemIndices bloco (matriz !! colunas)]  
            



{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todas as escadas.

== Exemplos

>>> posicaoBlocosesc (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma,Alcapao]]
[(1.0,0.0),(1.0,1.0)]
-}
posicaoBlocosvaz :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosvaz _ [] = []
posicaoBlocosvaz (x, y) (h:t) = posicaovaz (x, y) h ++ posicaoBlocosvaz (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição das escadas.

== Exemplos

>>>posicaovaz (0.0,0.0) [Vazio,Escada,Plataforma,Alcapao]
[(0.0,0.0)] 
-}
posicaovaz :: Posicao -> [Bloco] -> [Posicao]
posicaovaz _ [] = []
posicaovaz (x, y) (h:t)
    | h == Vazio = (x, y) : posicaovaz (x + 1, y) t
    | otherwise = posicaovaz (x + 1, y) t 

