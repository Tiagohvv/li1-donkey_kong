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
import Tarefa3 



atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza (h:t) acao jogo | acao == Just Subir && (estaEmEscada (mapa jogo) (jogador jogo) || verificaEscadaAbaixo (mapa jogo) (jogador jogo)) = jogo {jogador = velocidadesobe (jogador jogo) }
                         | acao == Just Descer && ((estaEmEscada (mapa jogo) (jogador jogo) && not (verificaPlataforma (mapa jogo) (jogador jogo))) || estaEscadaEPlat (mapa jogo) (jogador jogo) || not (verificaPlataforma (mapa jogo) (jogador jogo))) = jogo {jogador = velocidadedesce (jogador jogo) }
                         | acao == Just AndarDireita && (not (estaEmEscada (mapa jogo) (jogador jogo)) || (estaEmEscada (mapa jogo) (jogador jogo) && verificaPlataforma (mapa jogo) (jogador jogo))) = jogo {jogador = velocidadeDireita (jogador jogo) }
                         | acao == Just AndarDireita && ressalta (jogador jogo) == True = undefined 
                         | acao == Just AndarEsquerda && (not (estaEmEscada (mapa jogo) (jogador jogo)) || (estaEmEscada (mapa jogo) (jogador jogo) && verificaPlataforma (mapa jogo) (jogador jogo))) = jogo {jogador = velocidadeEsquerda (jogador jogo) }
                         | acao == Just AndarDireita && ressalta (jogador jogo) == True = undefined   
                         | acao == Just Saltar && verificaPlataforma (mapa jogo) (jogador jogo) && emEscada (jogador jogo) == False = jogo {jogador = saltar (jogador jogo)}     
                         | acao == Just Parar = jogo {jogador = parar (jogador jogo)}  
                         | h == Nothing = jogo {inimigos = andarInimigos (inimigos jogo) }                 
                         | otherwise = jogo 
 
                                         





andarInimigos :: [Personagem] -> [Personagem]
andarInimigos [] = []
andarInimigos (h:t) | tipo h == Fantasma && direcao h == Este = (h {velocidade = (2, snd (velocidade h))}) : andarInimigos t
                    | tipo h == Fantasma && direcao h == Oeste = (h {velocidade = (-2, snd (velocidade h))}) : andarInimigos t
                    | otherwise = (h:t)


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




