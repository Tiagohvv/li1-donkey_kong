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

{-
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza  acoesInimigos [per,per3] x jogo = undefined 


eventHandler :: Event -> Acao -> IO Acao
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) = return $ Just AndarDireita
eventHandler (EventKey (SpecialKey KeyRight) Up _ _) = return $ Just Parar
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) = return $ Just AndarEsquerda
eventHandler (EventKey (SpecialKey KeyLeft) Up _ _) = return $ Just Parar
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) = return $ Just Subir
eventHandler (EventKey (SpecialKey KeyUp) Up _ _) = return $ Just Parar
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) = return $ Just Descer
eventHandler (EventKey (SpecialKey KeyDown) Up _ _) = return $ Just Parar
eventHandler (EventKey (SpecialKey KeySpace) Down _ _) = return $ Just Saltar

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

criaHitboxBlocos :: [Posicao] -> [Hitbox]
criaHitboxBlocos ((x,y):t)= (x +1, y+1) : criaHitboxBlocos t 



criaHitbox :: Posicao -> Personagem -> Hitbox
criaHitbox (x,y) tamanho Personagem = ((x,y), (x + (fst (tamanho Personagem ))), (y + (snd (tamanho Personagem ))))


{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>
-}

naEscada :: Personagem -> [Posicao] -> Bool
naEscada (personagem) bloco = colisao (criaHitbox (posicao personagem) (tamanho personagem)) bloco

emAlcapao :: Personagem -> [Posicao] -> Bool
naAlcapao (personagem) bloco = undefined


ressaltando :: Personagem -> Maybe Direcao
ressaltando (inimigos) | ((fst (posicao inimigos)) + (fst (tamanho inimigos))) == 0 = Este
                       | ((fst (posicao inimigos)) + (fst (tamanho inimigos))) == 100 = Oeste 
                       | otherwise = Nothing


acoesInimigos :: [Personagem] -> [Maybe Acao]
acoesInimigos _ [] = []
acoesInimigos (h:t) | ressaltando h == Este = (direcao h == Este) : acoesInimigos t    
                    | ressaltando t == Oeste = (direcao h == Oeste) : acoesInimigos t 
                    | otherwise = Nothing : acoesInimigos t

-}