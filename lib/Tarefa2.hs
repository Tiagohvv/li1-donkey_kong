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
                                                  | ressalta jogador == True || validaressalta inimigos == False = False 
                                                  | validaposicaoJogador jogador inimigos == False = False
                                                  | containimigos inimigos < 2 = False 
                                                  | validavidas inimigos == False = False
                                                  | validaesc mapa == False = False
                                                  | otherwise = True




--

validaposicaoJogador :: Personagem -> [Personagem] -> Bool
validaposicaoJogador x [] = True
validaposicaoJogador x (h:t) | colisoesPersonagens x h == True = False
                             | otherwise = validaposicaoJogador x t

{-| Função que dada uma lista de personagens conta o seu número
== Exemplos

>>>containimigos [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}   ,Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este} ]
2

-}
containimigos :: [Personagem] -> Int
containimigos [] = 0
containimigos (h:t) = 1 + containimigos t


{-| Função que dada uma lista de personagens verifica se estas têm a propriedade vida igual a 1, caso contrário devolve false 
== Exemplos

>>>validavidas [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}   ,Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este} ]
True

-}
validavidas :: [Personagem] -> Bool
validavidas [] = True
validavidas (h:t) | (vida h) > 1 = False                                                   
                  | otherwise = validavidas t


{-| Função que dada uma lista de personagens verifica se estas têm a propriedade ressalta a false, casa alguma não tenha esta devolve false
== Exemplos

>>> validaressalta [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}   ,Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este} ]
True


-}
validaressalta :: [Personagem] -> Bool
validaressalta [] = True
validaressalta (h:t) | (ressalta h) == False = False                                                   
                     | otherwise = validaressalta t

{-| Função que verifica se a última lista de blocos apenas contém plataformas
 
== Exemplos

>>>validachao (Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]])
True

-}
validachao :: Mapa -> Bool
validachao (Mapa _ _ matriz) | all (==Plataforma) (last matriz) = True
                             | otherwise = False


{-| Função que dado um mapa devolve True, apenas se pelo menos uma das extremidades das escadas começar em plataforma
== Exemplos

>>>validaesc (Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]])
True
-}
validaesc :: Mapa -> Bool
validaesc (Mapa _ _ x)  | validaCoisasAl (posicaoBlocosesc (0,0) x) (posicaoBlocosal (0,0) x) == False = False
                        | escadinhas (validaCoisasPlat (posicaoBlocosesc (0.0,0.0) (x)) (posicaoBlocospl (0.0,0.0) (x))) (posicaoBlocosesc (0.0,0.0) (x)) == False = False
                        | otherwise = True  



{-| Função que dadas duas listas de posições aplica a função escadinhasaux usando as mesmas devolvendo false se esta função der false para qualquer das posiçoes do primeiro argumento

== Exemplos

>>> escadinhas [(0.0,0.0),(10.0,10.0),(10.0,0.0),(20.0,10.0)] [(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
False
-}
escadinhas :: [Posicao] -> [Posicao] -> Bool
escadinhas _ [] = False
escadinhas [] _ = True
escadinhas ((h,r):t) y | escadinhasaux (h,r) y == True = escadinhas t y
                       | otherwise = False


{-| Função que dada a posição de um bloco verifica se este começa ou acaba em algum da lista de blocos dada no segundo argumento

== Exemplos

>>>escadinhasaux (0.0,0.0) [(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
False

-}
escadinhasaux :: Posicao -> [Posicao] -> Bool
escadinhasaux _ [] = False
escadinhasaux (h,r) ((x,z):y) | (h,r) == (x,z+1) || (h,r) == (x,z-1) = True
                              | otherwise = escadinhasaux (h,r) y



{-| Função que aplica a função validaCoisasPlataux a todos os elementos da lista do primeiro argumento e a lista dada no segundo argumento

== Exemplos

>>>validaCoisasPlat [(0.0,0.0),(10.0,10.0),(10.0,0.0),(20.0,10.0)] [(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
[(0.0,0.0),(10.0,10.0),(10.0,0.0),(20.0,10.0)]
-}
validaCoisasPlat :: [Posicao] -> [Posicao] -> [Posicao]
validaCoisasPlat [] _ = []
validaCoisasPlat ((h,r):t) y = validaCoisasPlataux (h,r) y ++ validaCoisasPlat t y


{-| Função que dada a posição de um bloco e uma lista de posiçõse de blocos verifica se existe algum da segunda lista que está em cima ou em baixo do primeiro, caso isto não aconteça devolve uma lista vazia

== Exemplos

>>> validaCoisasPlataux (0.0,0.0) [(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
[(0.0,0.0)]

-}
validaCoisasPlataux :: Posicao -> [Posicao] -> [Posicao]
validaCoisasPlataux (h,r) [] = [(h,r)]
validaCoisasPlataux (h,r) ((x,z):y) | (h,r) == (x,z+1) || (h,r) == (x,z-1) = []
                                    | otherwise = validaCoisasPlataux (h,r) y


{-| Função que dada a uma lista de posições aplica a essa lista a função validaCoisasAlaux, dando lhe como primeiro argumento a cabeça da primeira lista e como segundo a segunda lista, se o resultado desta função for false a função  validaCoisasAl devolve false caso contrário repete a função para a cauda da primeira lista e se em nenhum caso a função validaCoisasAlaux devolver false então a função devolve true

== Exemplos

>>> validaCoisasAl [(0.0,0.0),(10.0,10.0),(10.0,0.0),(20.0,10.0)] [(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
True

-}
validaCoisasAl :: [Posicao] -> [Posicao] -> Bool
validaCoisasAl [] [] = True
validaCoisasAl [] _ = True
validaCoisasAl ((h,r):t) y | validaCoisasAlaux (h,r) y == True = validaCoisasAl t y
                           | otherwise = False


{-| Função que dada a posição de um bloco e uma lista de posiçõse de blocos verifica se existe algum desta lista que está em cima ou em baixo do primeiro, caso isto não acontecça devolve false

== Exemplos

>>> validaCoisasAlaux (0.0,0.0) [(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
True

-}
validaCoisasAlaux :: Posicao -> [Posicao] -> Bool
validaCoisasAlaux _ [] = True
validaCoisasAlaux (h,r) ((x,z):y) | (h,r) == (x,z+1) || (h,r) == (x,z-1) = False
                                  | otherwise = validaCoisasAlaux (h,r) y



{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todas as escadas

== Exemplos

>>> posicaoBlocosesc (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma,Alcapao]]
[(1.0,0.0),(1.0,1.0)]
-}
posicaoBlocosesc :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosesc _ [] = []
posicaoBlocosesc (x, y) (h:t) = posicaoesc (x, y) h ++ posicaoBlocosesc (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição das escadas.

== Exemplos

>>>posicaoesc (0.0,0.0) [Vazio,Escada,Plataforma,Alcapao]
[(1.0,0.0)]
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



{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os vazios.

== Exemplos

>>> posicaoBlocosvaz (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma,Alcapao]]
[(0.0,0.0),(0.0,1.0)]

-}
posicaoBlocosvaz :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocosvaz _ [] = []
posicaoBlocosvaz (x, y) (h:t) = posicaovaz (x, y) h ++ posicaoBlocosvaz (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição dos vazios.

== Exemplos

>>>posicaovaz (0.0,0.0) [Vazio,Escada,Plataforma,Alcapao]
[(0.0,0.0)] 
-}
posicaovaz :: Posicao -> [Bloco] -> [Posicao]
posicaovaz _ [] = []
posicaovaz (x, y) (h:t)
    | h == Vazio = (x, y) : posicaovaz (x + 1, y) t
    | otherwise = posicaovaz (x + 1, y) t 






posicaoBlocos :: Bloco -> [[Bloco]] -> [Posicao] 
posicaoBlocos bloco matriz = [(fromIntegral linhas, fromIntegral colunas) | 
                                   colunas <- [0 .. (length matriz)-1]  
                                 , linhas  <- elemIndices bloco (matriz !! colunas)]  