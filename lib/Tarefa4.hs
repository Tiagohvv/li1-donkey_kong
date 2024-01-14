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
import Tarefa3 (verificaPlataforma)


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza a acao jogo | acao == Just Subir && estaEmEscada (mapa jogo) (jogador jogo) = jogo {jogador = velocidadesobe (jogador jogo) }
                     | acao == Just Descer && estaEmEscada (mapa jogo) (jogador jogo)  = jogo {jogador = velocidadedesce (jogador jogo) }
                     | acao == Just AndarDireita && not (estaEmEscada (mapa jogo) (jogador jogo)) = jogo {jogador = velocidadeDireita (jogador jogo) }
                     | acao == Just AndarDireita && ressalta (jogador jogo) == True = undefined 
                     | acao == Just AndarEsquerda && not (estaEmEscada (mapa jogo) (jogador jogo)) = jogo {jogador = velocidadeEsquerda (jogador jogo) }
                     | acao == Just AndarDireita && ressalta (jogador jogo) == True = undefined   
                     | acao == Just Saltar && verificaPlataforma (mapa jogo) (jogador jogo) && emEscada (jogador jogo) == False = jogo {jogador = saltar (jogador jogo)}     
                     | acao == Just Parar = jogo {jogador = parar (jogador jogo)}                   
                     | otherwise = jogo 


{-|Função que verifica se uma personagem está a colidir com uma escada-}
--Função que verifica se uma personagem está a colidir com uma escada
estaEmEscada :: Mapa -> Personagem -> Bool 
estaEmEscada a@(Mapa _ _ blocos) p | blocoNaPosicao a (posicao p) == Just Escada = True
                                   | otherwise = False  

{-|Função que altera a velociadade da personagem para (0,0)-}                                         
-- Função que altera a velociadade da personagem para 0
parar :: Personagem -> Personagem 
parar p = p{velocidade = (0,0)}

{-|Função que altera a segunda componente da velocidade de uma personagem para -5 -} 
saltar :: Personagem -> Personagem 
saltar p = p {velocidade = (fst (velocidade p), -5)}

{-|Função que dada uma personagem altera a direção da personagem se esta for este para oeste e vice-versa -} 
andaparatras :: Personagem -> Personagem 
andaparatras p | direcao p == Este = p{direcao = Oeste}
               | direcao p == Oeste = p{direcao = Este} 
               | otherwise = p 
{-|Função que dada uma personagem altera a velociadade da personagem para -3 se a direção for este ou oeste e altera a direção para oeste se a direção da personagem for este, caso contrário devolve a personagem sem alterações -} 
velocidadeEsquerda :: Personagem -> Personagem 
velocidadeEsquerda p | direcao p == Oeste = p{velocidade = (-3,snd (velocidade p))}
                     | direcao p == Este = p{direcao = Oeste, velocidade = (-3,snd (velocidade p))}
                     | otherwise = p 
{-|Função que dada uma personagem altera a velociadade da personagem para 3 se a direção for este ou oeste e altera a direção para este se a direção da personagem for oeste, caso contrário devolve a personagem sem alterações -} 
velocidadeDireita :: Personagem -> Personagem 
velocidadeDireita p | direcao p == Este = p {velocidade= (3,snd (velocidade p))}
                    | direcao p == Oeste = p {direcao = Este, velocidade= (3,snd (velocidade p))}
                    |otherwise = p  

{-|Função que dada uma personagem altera a segunda componente da velocidade para -3 -} 
velocidadesobe :: Personagem -> Personagem 
velocidadesobe p = p {velocidade = (0,-3)} 

{-|Função que dada uma personagem altera a primeira componente da velocidade para 3 -} 
velocidadedesce :: Personagem -> Personagem 
velocidadedesce p = p {velocidade = (0,3)} 



--estaEscadaEPlat :: Mapa -> Personagem -> Bool 
--estaEscadaEPlat a@(Mapa _ _ blocos) p | verificaPlataforma a p  







{-
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

-}