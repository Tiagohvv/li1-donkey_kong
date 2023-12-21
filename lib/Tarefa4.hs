{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import LI12324
import Data.Bool (Bool)

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza [per,per3] per2 jogo = atualizaPersonagem per2


per2 = Personagem {
   posicao = (2,2),
   tamanho = (3,3),
   ressalta = False,
   tipo = Jogador
}

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





{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>colisao ((1,1),(2,2))  [((1.5,1.5),(3.5,3.5)), ((1.5,1.5),(3.5,3.5))]
True
-}


colisao :: Hitbox -> [Hitbox] -> Bool
colisao _ [] = True
colisao ((p1,p2),(p3,p4)) (((p5,p6),(p7,p8)):t) | sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8)) == False = False
                                                | otherwise = colisao ((p1,p2),(p3,p4)) t




{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>
-}

naEscada :: Personagem -> [Posicao] -> Bool
naEscada (personagem) bloco = colisao (criaHitbox (posicao personagem) (tamanho personagem)) bloco

emAlcapao :: Personagem -> [Posicao] -> Bool
emAlcapao (personagem) bloco = undefined


ressaltando :: Personagem -> Maybe Acao
ressaltando (inimigos) | ((fst (posicao inimigos)) + (fst (tamanho inimigos))) == 0 = Just AndarDireita
                       | ((fst (posicao inimigos)) + (fst (tamanho inimigos))) == 100 = Just AndarEsquerda 
                       | otherwise = Nothing


{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>atualizaPersonagem Just Subir
Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.0,4.0), direcao = Norte, tamanho = (10.0,20.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,0.0)}

-}
atualizaPersonagem :: Maybe Acao -> Personagem
atualizaPersonagem x | x == Just Subir = per2 {direcao = Norte}
                     | x == Just Descer = per2 {direcao = Sul}
                     | x == Just AndarDireita = per2 { direcao = Este}
                     | x == Just AndarEsquerda = per2 { direcao = Oeste}
                     | x == Just Saltar = per2 {direcao = Norte}
                     | x == Just Parar = per2 {velocidade = (0.0,0.0)}
                     | otherwise = per2            


atualizaInimigos :: Personagem -> Personagem
atualizaInimigos x | ressaltando x == Just Subir = x {direcao = Norte}
                   | ressaltando x == Just Descer = x {direcao = Sul}
                   | otherwise = per2                                 