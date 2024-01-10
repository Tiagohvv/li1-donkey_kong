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
import Tarefa1
import Tarefa2

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
--atualiza  acoesInimigos [per,per3] x jogo = undefined 
atualiza y x jogo = jogo {
    jogador = atualizaPersonagem x
    ,inimigos = atualizaInimigos (ressaltando [per,per3]) ([per,per3])  }


{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>>
-}

{--
naEscada :: Personagem -> [Posicao] -> Bool
naEscada (personagem) bloco = colisao (criaHitbox (posicao personagem) (tamanho personagem)) bloco

-}

ressaltando :: [Personagem] -> [Maybe Acao]
ressaltando [] = []
ressaltando (h:t) | ((fst (posicao h)) + (fst (tamanho h))) == 0 = Just AndarDireita : ressaltando t 
                  | ((fst (posicao h)) + (fst (tamanho h))) == 42 = Just AndarEsquerda : ressaltando t 
                  | otherwise = ressaltando t 


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




atualizaInimigos :: [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos (x:y) (h:t) | x == Just AndarDireita = h { direcao = Este} : atualizaInimigos y t 
                             | x == Just AndarEsquerda = h { direcao = Oeste} : atualizaInimigos y t 
                             | otherwise = h : atualizaInimigos y t                                


