{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1 
import GHC.Base (undefined)

mapaTeste = Mapa ((4,10), Oeste) (13,3) matrizJogoExp

matrizJogoExp :: [[Bloco]]
matrizJogoExp =[
     [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio,Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio,Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio,Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]


jogoexp :: Jogo 
jogoexp = Jogo mapaTeste inimigo colec player{posicao = pmapa, direcao = dmapa} False 
                                               where (Mapa (pmapa,dmapa) _ _) = mapaTeste
  



colec :: [(Colecionavel, Posicao)]
colec = [(Moeda, (2,6)), (Martelo, (2,16)),(Vida,(28,12)),(Moeda, (10,15)),(Moeda, (20,12)),(Moeda, (8,7)),(Moeda, (24,6)),(Chave, (27,16)), (Porta,(12.5,2)), (Porta,(17.5,2))]

inimigo :: [Personagem]
inimigo = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (4,6), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False }
                    ,
                    
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (11,8), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False}  
                    ,
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (16,16), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False} 
                    , 
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (19,12), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este,
                    temChave = False} 
                    , 
          Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (15,2.2), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste,
                    temChave = False}           
                    ]


player :: Personagem
player = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (5,2),
                     direcao = Este,
                     tamanho = (1,1), 
                     emEscada = False,
                     ressalta = False,
                     vida = 3,
                     pontos = 0,
                     aplicaDano = (False,0.0),                    
                     temChave = False}

{-| Função que dada uma personagem cria a sua hitbox.

== Exemplos

>>> gethitbox per2
((3.0,4.0),(13.0,24.0))

-}
gethitbox :: Personagem -> Hitbox 
gethitbox l = ((fst (posicao l) - fst (tamanho l)/2, snd (posicao l) - snd (tamanho l)/2 ),(fst (posicao l) + fst(tamanho l)/2, snd (posicao l) + snd (tamanho l)/2)) 

{-| Função que dada uma posição cria a hitbox de um colecionável
== Exemplos

>>>gethitboxcol (1,1)
((0.5,0.5),(1.5,1.5))

-}
gethitboxcol :: Posicao -> Hitbox 
gethitboxcol l = ((fst l - fst tamanho/2, snd l - snd tamanho/2) ,(fst l + fst tamanho /2, snd l + snd tamanho/2))
                      where tamanho = (1,1)

{-| Função que dada uma personagem devolve a sua hitbox de dano consoante a direção da mesma
== Exemplos

>>>getdamagehitbox player
((1.5,0.5),(2.5,1.5))

-}
getdamagehitbox :: Personagem -> Hitbox
getdamagehitbox p 
  | direcao p == Este = getdamagehitboxAuxEste (gethitbox p)
  | direcao p == Oeste = getdamagehitboxAuxOeste (gethitbox p)
  | direcao p == Norte = getdamagehitboxAuxNorte (gethitbox p)
  | direcao p == Sul =  getdamagehitboxAuxSul (gethitbox p)

{-| Função que dada uma hitbox cria a hitbox que aplica dano a direita

== Exemplos

>>>getdamagehitboxAuxEste ((0.5,0.5),(1.5,1.5))
((1.5,0.5),(2.5,1.5))

-}
getdamagehitboxAuxEste :: Hitbox -> Hitbox 
getdamagehitboxAuxEste ((x,y),(x2,y2)) = ((x+(x2-x),y),(x2+(x2-x),y2))

{-| Função que dada uma hitbox cria a hitbox que aplica dano a esquerda

== Exemplos

>>>getdamagehitboxAuxOeste ((4,3.5),(5,4.5))
((3.0,3.5),(4.0,4.5))

-}
getdamagehitboxAuxOeste :: Hitbox -> Hitbox 
getdamagehitboxAuxOeste ((x,y),(x2,y2)) = ((x-(x2-x),y),(x2-(x2-x),y2))


{-| Função que dada uma hitbox cria a hitbox que aplica dano a cima

== Exemplos

>>>getdamagehitboxAuxNorte ((0.5,0.5),(1.5,1.5))
((0.5,-0.5),(1.5,0.5))

-}
getdamagehitboxAuxNorte :: Hitbox -> Hitbox 
getdamagehitboxAuxNorte ((x,y),(x2,y2)) = ((x,y-(y2-y)),(x2,y2-(y2-y)))

{-| Função que dada uma hitbox cria a hitbox que aplica dano a baixo
== Exemplos

>>>getdamagehitboxAuxSul ((0.5,0.5),(1.5,1.5))
((0.5,1.5),(1.5,2.5))

-}
getdamagehitboxAuxSul :: Hitbox -> Hitbox 
getdamagehitboxAuxSul ((x,y),(x2,y2)) = ((x,y+(y2-y)),(x2,y2+(y2-y)))  

{-| Função que dado um jogo aplica a função inimigomorre aos inimigos do jogo-}
-- inimigo morre quando a vida chega a 0 
inimigomorreEMjogo :: Jogo -> Jogo 
inimigomorreEMjogo j = j {inimigos = inimigomorre (inimigos j)}

{-| Função dada uma lista de personagens aplica a função inimigomorreAux a essa lista
== Exemplos

>>>inimigomorre [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}   ,Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este} ]
[Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (6.0,12.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]

-}
inimigomorre :: [Personagem] -> [Personagem]
inimigomorre [] = []
inimigomorre (h:t)  =  inimigomorreAux h : inimigomorre t

{-| Função que dada uma personagem aplica a função xaupersonagem se esta for do tipo fantasma e a sua vida for igual a 0 caso contrário devolve a mesma personagem
== Exemplos

>>>inimigomorreAux Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}
inimigomorreAux :: Personagem -> Personagem 
inimigomorreAux p | tipo p == Fantasma && vida p == 0 = p { posicao = xaupersonagem (posicao p)}
                  | otherwise = p 

{-| Função que dada uma posição acrescenta 1000 ao x e y
== Exemplos

>>>xaupersonagem (4,3)
(1004,1003)
-}
xaupersonagem :: Posicao -> Posicao
xaupersonagem (x,y) = (x+1000,y+1000) 

{-| Função que dado um tempo e um jogo aplica a função temGravidade ao jogador recebendo como primeiro argumento o tempo,o segundo o jogador e como terceiro o mapa-}
temGravidadeEMjogo :: Tempo -> Jogo -> Jogo 
temGravidadeEMjogo t j = j {jogador = temGravidade t (jogador j) (mapa j) } 

{-| Função que 
== Exemplos

>>> temGravidade 10 (Personagem {velocidade = (0,0),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (1,1),  emEscada = False,ressalta = False, vida = 1,pontos = 0,aplicaDano = (True,0.0)}) ((Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]))
Personagem {velocidade = (0.0,100.0), tipo = Jogador, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (True,0.0)}

-}
temGravidade :: Tempo -> Personagem -> Mapa -> Personagem                    
temGravidade t p a@(Mapa _ _ matriz) | (blocoNaPosicao a (fst (posicao p), snd (posicao p) +0.5) /= Just Plataforma && blocoNaPosicao a (fst (posicao p), snd (posicao p) +0.5) /= Just Alcapao) && not (estaEmEscada a p) = velocidadeGravidade t p 
                                     | otherwise = p {velocidade = (fst(velocidade p),if (snd(velocidade p) < 0) || (estaEmPlataforma a p && (snd(velocidade p) > 0)) || (estaEscadaEPlat a p && snd(velocidade p) > 0) || (estaEmEscada a p && not (verificaPlataforma a p) && snd(velocidade p) > 0) || (verificaPlataAcima a p && verificaEscadaAbaixo a p && snd(velocidade p) < 0 )  then snd(velocidade p) else 0)} 

{-| Função que dado um tempo e uma personagem devolve a personagem com a velocidade atualizada, fazendo isto ao acrescentar a segunda componente do par da velocidade a velocidade multiplicada pelo tempo
== Exemplos

>>>velocidadeGravidade 10 (Personagem {velocidade = (0,0),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (1,1),  emEscada = False,ressalta = False, vida = 1,pontos = 0,aplicaDano = (True,0.0)})
Personagem {velocidade = (0.0,100.0), tipo = Jogador, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (True,0.0)}

-}
velocidadeGravidade :: Tempo -> Personagem -> Personagem 
velocidadeGravidade t p = p {velocidade = (fst (velocidade p) , (snd (velocidade p)) + ((snd gravidade) * t))}

{-| Função que dado um mapa e uma personagem verifica se esta está em cima de uma plataforma-}
verificaPlataforma :: Mapa -> Personagem -> Bool 
verificaPlataforma a@(Mapa _ _ blocos) p | blocoNaPosicao a (fst(posicao p), (snd (posicao p))+0.5) == Just Plataforma = True 
                                         | otherwise = False 

{-| Função que dado um mapa e uma personagem verifica se o bloco em que esta se encontra corresponde a uma escada
== Exemplos

>>>estaEmEscada mapa
True

-}
estaEmEscadaEmJogo :: Jogo -> Jogo 
estaEmEscadaEmJogo j = j {jogador = poeEmEscada (mapa j) (jogador j)} 

estaEmEscada :: Mapa -> Personagem -> Bool 
estaEmEscada a@(Mapa _ _ blocos) p | blocoNaPosicao a (posicao p) == Just Escada = True
                                   | otherwise = False 

poeEmEscada :: Mapa -> Personagem -> Personagem 
poeEmEscada a@(Mapa _ _ blocos) p | estaEmEscada a p = p {emEscada = True}
                                  | otherwise = p {emEscada = False}                                  

estaEmPlataforma :: Mapa -> Personagem -> Bool 
estaEmPlataforma a@(Mapa _ _ blocos) p | blocoNaPosicao a (posicao p) == Just Plataforma = True
                                       | otherwise = False    

estaEscadaEPlat :: Mapa -> Personagem -> Bool 
estaEscadaEPlat a@(Mapa _ _ blocos) p | (verificaPlataforma a p) && (verificaEscadaAbaixo a p) = True 
                                      | otherwise = False                                                                 
verificaEscadaAbaixo :: Mapa -> Personagem -> Bool 
verificaEscadaAbaixo a@(Mapa _ _ blocos) p | (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1.7) == Just Escada) || (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+0.7)== Just Escada) = True 
                                           | otherwise = False

verificaPlataAcima :: Mapa -> Personagem -> Bool 
verificaPlataAcima a@(Mapa _ _ blocos) p | blocoNaPosicao a (fst(posicao p), (snd (posicao p))-0.7) == Just Plataforma = True 
                                         | otherwise = False                                          



{-| Função que dado um jogo aplica a função perdevidainimigoEmjogo aos inimigos-}
-- Fantasma perde vida ao ser martelado 
perdevidainimigoEMjogo :: Jogo -> Jogo 
perdevidainimigoEMjogo j = j {inimigos = perdevidainimigo (inimigos j) (jogador j) } 

{-| Função que recebe uma lista de personagens e uma personagem, caso a segunda personagem tenha estiver a  a True e as hitboxes desse personagem e do primeiro personagem da lsita recebida no primeiro argumento se tocarem, então a função irá devolver  
== Exemplos

>>>perdevidainimigo [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste},Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este}] (Personagem {velocidade = (0,0),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (1,1),  emEscada = False,ressalta = False, vida = 1,pontos = 0,aplicaDano = (True,0.0)})
[Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (6.0,12.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]

-}
perdevidainimigo :: [Personagem] -> Personagem -> [Personagem] 
perdevidainimigo [] _ = [] 
perdevidainimigo (ini:inis) j  = if ((aplicaDano j == (True, snd (aplicaDano j))) && (sobreposicao (getdamagehitbox j) (gethitbox ini) == True) )
                                 then ini {vida = (vida ini)-1} : perdevidainimigo inis j 
                                 else ini : perdevidainimigo inis j 

 {-| Função que dado o jogo aplica a função perdevidaJogador ao jogador-}  
--jogador perde vida ao ser tocado pelo fantasma          
perdevidaJogadorEMjogo :: Jogo -> Jogo 
perdevidaJogadorEMjogo j = j {jogador = perdevidaJogador (mapa j) (jogador j) (inimigos j) }


{-| Função que recebe uma personagem no primeiro argumento e uma lista de personagens no segundo e caso haja colisão da primeira personagem com uma das da lista, a função devolve a primeira personagem com menos uma vida
== Exemplos

>>>perdevidaJogador (Personagem {velocidade = (0,0),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (1,1),  emEscada = False,ressalta = False, vida = 1,pontos = 0,aplicaDano = (True,0.0)}) [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste},Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este}]
Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (True,0.0)}

-}
perdevidaJogador :: Mapa -> Personagem -> [Personagem] -> Personagem 
perdevidaJogador _ j [] = j                          
perdevidaJogador a@(Mapa (p1,p2) _ _) j (ini:inis) | colisoesPersonagens j ini = perdevidaJogador a (j {vida = (vida j)-1, posicao =p1, direcao = p2}) inis 
                                                 | otherwise = perdevidaJogador a j inis 

{-| Função que dado um jogo aplica a função xaucolec e armaEpontosJogador-}
-- arma o jogador se for martelo e aumenta pontos se for moeda. Desaparecem se forem recolhidos 
armaEpontosJogadorEMjogo :: Tempo -> Jogo -> Jogo 
armaEpontosJogadorEMjogo t j = j {colecionaveis = xaucolec (jogador j) (colecionaveis j), jogador = armaEpontosJogador t (jogador j) (colecionaveis j)}

{-| Função que dada uma personagem e uma lista de colecionáveis verifica se alguma personagem colide com esses colecionáveis, e caso isso aconteça retira o colecionável do mapa
== Exemplos

>>>xaucolec Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste} [(Martelo, (5,1)),(Moeda, (2,1))]
[(Martelo,(5.0,1.0)),(Moeda,(2.0,1.0))]

-}
xaucolec :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
xaucolec j [] = []
xaucolec j ((col,pos):cols) | (col== Porta) && (temChave j) && sobreposicao (gethitbox j) (gethitboxcol pos) = (col,xaupersonagem pos) : xaucolec j cols 
                            | (sobreposicao (gethitbox j) (gethitboxcol pos)) && (not (col==Porta))  = (col,xaupersonagem pos) : xaucolec j cols 
                            | otherwise = (col,pos) : xaucolec j cols       

{-| Função que dada uma personagem e uma lista de colecionáveis verifica se alguma personagem colide com esses colecionáveis, e caso isso aconteça atualiza o jogador
== Exemplos

>>>armaEpontosJogador Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste} [(Martelo, (5,1)),(Moeda, (2,1))]
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}
armaEpontosJogador :: Tempo -> Personagem -> [(Colecionavel, Posicao)] -> Personagem 
armaEpontosJogador t j [] = j{aplicaDano = (snd (aplicaDano j) >0,if snd (aplicaDano j )<= 0 then 0 else snd (aplicaDano j) -t)} 
armaEpontosJogador t j (col:cols) | sobreposicao (gethitbox j) (gethitboxcol (snd col))  &&  (fst col == Martelo) = armaEpontosJogador t (j {aplicaDano = (True, 10)}) cols
                                | sobreposicao (gethitbox j) (gethitboxcol (snd col)) &&  (fst col == Moeda) = armaEpontosJogador t (j {pontos = (pontos j) +1}) cols 
                                | sobreposicao (gethitbox j) (gethitboxcol (snd col)) &&  (fst col == Vida) = armaEpontosJogador t (j {vida = (vida j) +1}) cols 
                                | sobreposicao (gethitbox j) (gethitboxcol (snd col)) &&  (fst col == Chave) = armaEpontosJogador t (j {temChave = True}) cols
                                | otherwise = armaEpontosJogador t j cols 




{-| Função que dado um jogo aplica a função pisaalcapao ao mapa desse jogo-}

-- Função que faz um alçapão desaparecer se o jogador o pisar  
pisaalcapaoEMjogo :: Jogo -> Jogo 
pisaalcapaoEMjogo j = j {mapa = pisaalcapaoJogador (jogador j) (0,0) (mapa j)}                         


{-| Função que dada uma poiscao e um mapa verifica se essa posição está por cima de um alçapao 
== Exemplos

>>>pisaalcapao (1,1) (Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]])
False

-} 

{-| Função que dada uma personagem e um mapa retorna o mapa sem o alçapao se a personagem que está por cima do mesmo é do tipo jogador
== Exemplos

>>>pisaalcapaoJogador Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}  (Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) 
Mapa ((1.0,1.0),Oeste) (0.5,2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]

-}


pisaalcapaoJogador :: Personagem -> Posicao -> Mapa -> Mapa 
pisaalcapaoJogador  p (x,y) a@(Mapa (p1, d) p2 []) = a 
pisaalcapaoJogador  p (x,y) a@(Mapa (p1, d) p2 blocos)  = Mapa (p1, d) p2 (pisaalcapao2 p (x,y) blocos)               





{-| Função que recebe uma personagem e uam posiçao e a matriz inteira e aplica a pisaalcapao a todas as linhas da matriz
== Exemplos

>>>
-}
pisaalcapao2 :: Personagem -> Posicao -> [[Bloco]] -> [[Bloco]]
pisaalcapao2 _ _ [] = []
pisaalcapao2 p (x,y) (bloco:blocos) = pisaalcapao p (x,y) bloco : pisaalcapao2 p (x,y+1) blocos 

{-| Função que recebe uma personagem e uma posição e uma linha da matriz de blocos e retorna essa linha com os alçapoes substituidso por vazio caso o jogador esteja em colisão com a mesma 
== Exemplos

>>>
-}
pisaalcapao :: Personagem -> Posicao -> [Bloco] -> [Bloco] 
pisaalcapao _ _ [] = []
pisaalcapao p (x,y) (b1:b) | tipo p == Jogador && b1 == Alcapao && sobreposicao (gethitboxcol (x,y)) (gethitbox p) = (Vazio: (pisaalcapao p (x+1,y) b))
                                     | otherwise = (b1:pisaalcapao p (x+1,y) b)

{-| Função que dada uma velocidade, um tempo e uma posicao dá a posição atualizada conforme os parámetros anteriores
== Exemplos

>>>posicaoatualizada (1,1) 10 (10,15)
(20.0,25.0)

-}
-- Atualiza as posicoes com a velocidade 
posicaoatualizada :: Velocidade -> Tempo -> Posicao -> Posicao 
posicaoatualizada (v1,v2) t (x,y) = ((x+v1*t),(y+v2*t))  


{-| Função que dado um tempo e uma personagem aplica a função posicaoatualizada a posição do jogador utilizando os parámetros dados 
== Exemplos

>>>posicaoatualizadaPer (10.0) (Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste})  
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}

posicaoatualizadaPer :: Tempo -> Mapa -> Personagem -> Personagem 
posicaoatualizadaPer t a@(Mapa _ _ blocos) p = p {posicao= posicaoatualizada (velocidade p) t (posicao p) } 



{-| Função que dado um tempo e uma personagem aplica a função posicaoatualizada a posição do jogador utilizando os parámetros dados 
== Exemplos

>>>posicaoatualizadaPer (10.0) (Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste})  
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}

posicaoatualizadaIni :: Tempo -> Mapa -> [Personagem] -> [Personagem] 
posicaoatualizadaIni _ _ [] = []
posicaoatualizadaIni t a@(Mapa _ _ blocos) (i1:i2) = (i1 {posicao= posicaoatualizada (velocidade i1) t (posicao i1)}) : posicaoatualizadaIni t a i2 


{-| Função que dado um jogo aplica a função posicaoatualizadaPer a esse jogo-}
posicaoatualizadaPerEmjogo :: Tempo -> Jogo -> Jogo 
posicaoatualizadaPerEmjogo t j = j {jogador = posicaoatualizadaPer t (mapa j) (jogador j) , inimigos = posicaoatualizadaIni t (mapa j) (inimigos j) } 

limitesEmJogo :: Jogo -> Jogo 
limitesEmJogo j = j {inimigos = limitesIniTodos (mapa j) (inimigos j), jogador = limites (mapa j) (jogador j) } 

verificaIniO :: Mapa -> Personagem -> Bool 
verificaIniO a@(Mapa _ _ blocos) p | (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1) == Just Plataforma) && (blocoNaPosicao a ((fst(posicao p))-1, (snd (posicao p))+1) == Just Vazio) = True 
                                   | otherwise = False 

verificaIniE :: Mapa -> Personagem -> Bool 
verificaIniE a@(Mapa _ _ blocos) p | (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1) == Just Plataforma) && (blocoNaPosicao a ((fst(posicao p))+1, (snd (posicao p))+1) == Just Vazio) = True 
                                   | otherwise = False 
{-
limitesIni :: Mapa -> [Personagem] -> [Personagem]
limitesIni _ [] = []
limitesIni a@(Mapa _ _ (bloco:blocos)) (h:t) | ressalta h == True && ((fst (posicao h) >= fromIntegral (length bloco)) || verificaIniE a h) = h {velocidade = (fst (velocidade h)* (-1),snd (velocidade h)), direcao = Oeste } : limitesIni a t
                                             | ressalta h == True && ((fst (posicao h) <= 0) || verificaIniO a h) = h {velocidade = (fst (velocidade h)* (-1),snd (velocidade h)), direcao= Este} : limitesIni a t 
                                             | otherwise = (h:t)
-}

limitesIni :: Mapa -> Personagem -> Personagem
limitesIni a@(Mapa _ _ (bloco:blocos)) h | ressalta h == True && ((fst (posicao h) >= fromIntegral (length bloco)) || verificaIniE a h) = h {velocidade = (fst (velocidade h)* (-1),snd (velocidade h)), direcao = Oeste } 
                                         | ressalta h == True && ((fst (posicao h) <= 0) || verificaIniO a h) = h {velocidade = (fst (velocidade h)* (-1),snd (velocidade h)), direcao= Este}  
                                         | otherwise = h
limitesIniTodos :: Mapa -> [Personagem] -> [Personagem]
limitesIniTodos a@(Mapa _ _ (bloco:blocos)) l = map (limitesIni a) l 


limites :: Mapa -> Personagem -> Personagem 
limites a@(Mapa _ _ (bloco:blocos)) p | not (temChave p)&& (fst (posicao p)>=11.5 && fst (posicao p) <=18) && (snd (posicao p)>=0 && snd (posicao p)<=4) = p {posicao = (if fst (posicao p)<=13 then fst (posicao p)-0.3 else fst (posicao p)+0.5 , snd (posicao p))} 
                                      | (fst (posicao p) >= fromIntegral (length bloco)) = p {posicao = (fromIntegral (length bloco)-1, snd (posicao p))} 
                                      | (fst (posicao p) <= 0) = p {posicao = (0.5, snd (posicao p))} 
                                      | otherwise = p 
                                       

tiraposicoes :: Mapa -> [Double]
tiraposicoes (Mapa (a,b) c []) = [] 
tiraposicoes (Mapa (a,b) c (bloco:blocos)) = fromIntegral (length bloco) : tiraposicoes (Mapa (a,b) c blocos) 

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta x t jogoexp | pausa jogoexp = jogoexp  
                      | otherwise = temGravidadeEMjogo t $ posicaoatualizadaPerEmjogo t $ inimigomorreEMjogo $  pisaalcapaoEMjogo  $ perdevidainimigoEMjogo $ perdevidaJogadorEMjogo $ armaEpontosJogadorEMjogo t $ limitesEmJogo $ estaEmEscadaEmJogo  jogoexp
                      

