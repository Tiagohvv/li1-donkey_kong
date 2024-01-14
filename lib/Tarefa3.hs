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

mapaTeste = Mapa ((1,1), Oeste) (0.5, 2.5) matrizJogoExp

matrizJogoExp :: [[Bloco]]
matrizJogoExp =[
     [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Alcapao, Alcapao, Alcapao, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio ,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Escada, Plataforma,Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]



player2 :: Personagem
player2 = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (1,1),
                     direcao = Este,
                     tamanho = (2,2),
                     emEscada = False,
                     ressalta = False,
                     vida = 3,
                     pontos = 0,
                     aplicaDano = (True,0.0)
                     }



player3 :: Personagem
player3 = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (1.5,1),
                     direcao = Este,
                     tamanho = (1,1),
                     emEscada = False,
                     ressalta = False,
                     vida = 3,
                     pontos = 0,
                     aplicaDano = (True,0.0)
                     }



per = Personagem {
   posicao = (5,5),
   tamanho = (10,20),
   ressalta = True,
   tipo = Fantasma,
   vida = 1
}



per2 = Personagem {
   posicao = (10,2),
   tamanho = (3,3),
   ressalta = False,
   tipo = Jogador
}



per3 = Personagem {
   posicao = (4,4),
   tamanho = (3,3),
   ressalta = True,
   tipo = Fantasma,
   vida = 1
}

jogoexp :: Jogo 
jogoexp = Jogo mapaTeste inimigo colec player{posicao = pmapa, direcao = dmapa}
                                               where (Mapa (pmapa,dmapa) _ _) = mapaTeste  

colec :: [(Colecionavel, Posicao)]
colec = [(Moeda, (2,6)), (Martelo, (0,16))]

inimigo :: [Personagem]
inimigo = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (14,8), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}   
                    ,
                    
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (6,12), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este}          
                    ]


player :: Personagem
player = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (1,1),
                     direcao = Este,
                     tamanho = (1,1), 
                     emEscada = False,
                     ressalta = False,
                     vida = 1,
                     pontos = 0,
                     aplicaDano = (False,0.0)
                     }

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
temGravidade t p a@(Mapa _ _ matriz) | blocoNaPosicao a (fst (posicao p), snd (posicao p) +0.3) /= Just Plataforma && not (estaEmEscada a p) = velocidadeGravidade t p 
                                     | otherwise = p {velocidade = (fst(velocidade p),if (snd(velocidade p) < 0) || ((estaEmEscada a p)&& snd(velocidade p) > 0 ) then snd(velocidade p) else 0)} 


{-| Função que dado um tempo e uma personagem devolve a personagem com a velocidade atualizada, fazendo isto ao acrescentar a segunda componente do par da velocidade a velocidade multiplicada pelo tempo
== Exemplos

>>>velocidadeGravidade 10 (Personagem {velocidade = (0,0),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (1,1),  emEscada = False,ressalta = False, vida = 1,pontos = 0,aplicaDano = (True,0.0)})
Personagem {velocidade = (0.0,100.0), tipo = Jogador, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (True,0.0)}

-}
velocidadeGravidade :: Tempo -> Personagem -> Personagem 
velocidadeGravidade t p = p {velocidade = (fst (velocidade p) , (snd (velocidade p)) + ((snd gravidade) * t))}

{-| Função que dado um mapa e uma personagem verifica se esta está em cima de uma plataforma-}
verificaPlataforma :: Mapa -> Personagem -> Bool 
verificaPlataforma a@(Mapa _ _ blocos) p | blocoNaPosicao a (fst(posicao p), (snd (posicao p))+0.7) == Just Plataforma = True 
                                         | otherwise = False 

{-| Função que dado um mapa e uma personagem verifica se o bloco em que esta se encontra corresponde a uma escada
== Exemplos

>>>estaEmEscada mapa
True

-}
estaEmEscada :: Mapa -> Personagem -> Bool 
estaEmEscada a@(Mapa _ _ blocos) p | blocoNaPosicao a (posicao p) == Just Escada = True
                                   | otherwise = False 

{-}

aplicaGravidade :: Mapa -> Personagem -> Personagem
aplicaGravidade mapa personagem =
  if estaSobrePlataforma mapa personagem
    then personagem -- Não aplicar gravidade se estiver sobre uma plataforma
    else aplicarGravidade personagemvalidachao [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]
estaSobrePlataforma (Mapa _ _ blocos) personagem =
  let posicaoPersonagem = posicao personagem
      blocoAtual = blocoNaPosicao mapa posicaoPersonagem
  in blocoAtual == Just Plataforma

-- Encontra o bloco na posição dada no mapa
blocoNaPosicao :: Mapa -> Posicao -> Maybe Bloco
blocoNaPosicao (Mapa _ _ blocos) (x, y) =
  let linha = round y
      coluna = round x
  in if linha >= 0 && linha < length blocos && coluna >= 0 && coluna < length (head blocos)
        then Just (blocos !! linha !! coluna)
        else Nothing

-- Aplica o efeito de gravidade ao personagem
aplicarGravidade :: Personagem -> Personagem
aplicarGravidade personagem =
  let (vx, vy) = velocidade personagem
      novaVelocidade = (vx, vy - gravidade)
      novaPosicao = atualizarPosicao personagem novaVelocidade
  in personagem { velocidade = novaVelocidade, posicao = novaPosicao }


---Atualiza a posição do personagem com base na velocidade
atualizarPosicao :: Personagem -> Velocidade -> Posicao
atualizarPosicao personagem (vx, vy) =
  let (x, y) = posicao personagem
  in (x + vx, y + vy)
  

aplicaGravidade :: Mapa -> Personagem -> Personagem 
aplicaGravidade mp p | blocoNaPosicao  mp p == Just-- = novaPosicao p
                     | otherwise = p    

novaPosicao :: Posicao -> Posicao 
novaPosicao (x,y) = undefined


estaemPlataforma :: Mapa -> Personagem -> Bool
estaemPlataforma m@(Mapa _ _ blocos) p = blocoatual == Just Plataforma 
          where posicaoPersonagem = posicao p 
                blocoatual = blocoNaPosicao m posicaoPersonagem


blocoNaPosicao :: Mapa -> Posicao -> Maybe Bloco
blocoNaPosicao (Mapa _ _ blocos) (x, y) = 
    if linha >= 0 && linha < length blocos && coluna >= 0 && coluna < length (head blocos)
    then Just (blocos !! linha !! coluna)
                    tipo = Fantasma, 
    else Nothing
    where linha = round y
          coluna = round x 
-}

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
perdevidaJogadorEMjogo j = j {jogador = perdevidaJogador (jogador j) (inimigos j) }


{-| Função que recebe uma personagem no primeiro argumento e uma lista de personagens no segundo e caso haja colisão da primeira personagem com uma das da lista, a função devolve a primeira personagem com menos uma vida
== Exemplos

>>>perdevidaJogador (Personagem {velocidade = (0,0),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (1,1),  emEscada = False,ressalta = False, vida = 1,pontos = 0,aplicaDano = (True,0.0)}) [Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste},Personagem {velocidade = (0,0), tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (6,12), tamanho = (1,1), aplicaDano = (False, 0), direcao = Este}]
Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (True,0.0)}

-}
perdevidaJogador :: Personagem -> [Personagem] -> Personagem 
perdevidaJogador j [] = j                          
perdevidaJogador j (ini:inis) | colisoesPersonagens j ini = perdevidaJogador (j {vida = (vida j)-1}) inis 
                              | otherwise = perdevidaJogador j inis 

{-| Função que dado um jogo aplica a função xaucolec e armaEpontosJogador-}
-- arma o jogador se for martelo e aumenta pontos se for moeda. Desaparecem se forem recolhidos 
armaEpontosJogadorEMjogo :: Jogo -> Jogo 
armaEpontosJogadorEMjogo j = j {colecionaveis = xaucolec (jogador j) (colecionaveis j), jogador = armaEpontosJogador (jogador j) (colecionaveis j)}

{-| Função que dada uma personagem e uma lista de colecionáveis verifica se alguma personagem colide com esses colecionáveis, e caso isso aconteça retira o colecionável do mapa
== Exemplos

>>>xaucolec Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste} [(Martelo, (5,1)),(Moeda, (2,1))]
[(Martelo,(5.0,1.0)),(Moeda,(2.0,1.0))]

-}
xaucolec :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
xaucolec j [] = []
xaucolec j ((col,pos):cols) | sobreposicao (gethitbox j) (gethitboxcol pos) = (col,xaupersonagem pos) : xaucolec j cols 
                            | otherwise = (col,pos) : xaucolec j cols       

{-| Função que dada uma personagem e uma lista de colecionáveis verifica se alguma personagem colide com esses colecionáveis, e caso isso aconteça atualiza o jogador
== Exemplos

>>>armaEpontosJogador Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste} [(Martelo, (5,1)),(Moeda, (2,1))]
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}
armaEpontosJogador :: Personagem -> [(Colecionavel, Posicao)] -> Personagem 
armaEpontosJogador j [] = j 
armaEpontosJogador j (col:cols) | sobreposicao (gethitbox j) (gethitboxcol (snd col))  &&  (fst col == Martelo) = armaEpontosJogador (j {aplicaDano = (True, 10)}) cols
                                | sobreposicao (gethitbox j) (gethitboxcol (snd col)) &&  (fst col == Moeda) = armaEpontosJogador (j {pontos = (pontos j) +1}) cols  
                                | otherwise = armaEpontosJogador j cols 

{-| Função que dado um jogo aplica a função pisaalcapao ao mapa desse jogo-}

-- Função que faz um alçapão desaparecer se o jogador o pisar  
pisaalcapaoEMjogo :: Jogo -> Jogo 
pisaalcapaoEMjogo j = j {mapa = pisaalcapaoJogador (jogador j) (mapa j) }

{-| Função que dada uma poiscao e um mapa verifica se essa posição está por cima de um alçapao 
== Exemplos

>>>pisaalcapao (1,1) (Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]])
False

-}
pisaalcapao :: Posicao -> Mapa -> Bool 
pisaalcapao (x,y) a@(Mapa _ _ blocos) =  blocoNaPosicao a (x,y+1) == Just Alcapao 

{-| Função que dada uma personagem e um mapa retorna o mapa sem o alçapao se a personagem que está por cima do mesmo é do tipo jogador
== Exemplos

>>>pisaalcapaoJogador Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste}  (Mapa ((1,1), Oeste) (0.5, 2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) 
Mapa ((1.0,1.0),Oeste) (0.5,2.5) [[Escada,Vazio,Vazio],[Escada,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]

-}
pisaalcapaoJogador :: Personagem -> Mapa -> Mapa
pisaalcapaoJogador j m@(Mapa (p1,d) p2 []) = m
pisaalcapaoJogador j a@(Mapa (p1,d) p2 (linha1:resto)) | tipo j == Jogador && pisaalcapao (posicao j) a  = Mapa (p1,d) p2 (trocarBlocoNaPosicao Alcapao Vazio (fst (posicao j), snd (posicao j) +1) (linha1:resto))
                                                       | otherwise = a


{-| Função que dada uma
== Exemplos

>>>

-}
trocarBlocoNaPosicao :: Bloco -> Bloco -> Posicao -> [[Bloco]] -> [[Bloco]]
trocarBlocoNaPosicao _ _ _ [] = []  
trocarBlocoNaPosicao antigo novo (coluna, linha) matriz =
    take (round linha) matriz ++
    [trocarLinhaNaPosicao antigo novo coluna (matriz !! round linha)] ++
    drop (round linha + 1) matriz
  where
    trocarLinhaNaPosicao _ _ _ [] = []  
    trocarLinhaNaPosicao antigo novo coluna (b:bs)
      | coluna == 0 = novo : bs  
      | otherwise = b : trocarLinhaNaPosicao antigo novo (coluna - 1) bs 

{-| Função que dada uma velocidade, um tempo e uma posicao dá a posição atualizada conforme os parámetros anteriores
== Exemplos

>>>posicaoatualizada (1,1) 10 (10,15)
(20.0,25.0)

-}
posicaoatualizada :: Velocidade -> Tempo -> Posicao -> Posicao 
posicaoatualizada (v1,v2) t (x,y) = ((x+v1*t),(y+v2*t))  

{-| Função que dado um tempo e uma personagem aplica a função posicaoatualizada a posição do jogador utilizando os parámetros dados 
== Exemplos

>>>posicaoatualizadaPer (10.0) (Personagem {velocidade = (0,0),tipo = Fantasma, emEscada = False, vida = 1, pontos = 0, ressalta = True, posicao = (14,8), tamanho = (1,1),  aplicaDano = (False, 0), direcao = Oeste})  
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (14.0,8.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}
posicaoatualizadaPer :: Tempo -> Personagem -> Personagem 
posicaoatualizadaPer t p = p {posicao= posicaoatualizada (velocidade p) t (posicao p)}

{-| Função que dado um jogo aplica a função posicaoatualizadaPer a esse jogo-}
posicaoatualizadaPerEmjogo :: Tempo -> Jogo -> Jogo 
posicaoatualizadaPerEmjogo t j = j {jogador = posicaoatualizadaPer t (jogador j)} 



movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta x t jogoexp = temGravidadeEMjogo t $ posicaoatualizadaPerEmjogo t $ inimigomorreEMjogo  $ perdevidainimigoEMjogo $ perdevidaJogadorEMjogo $ armaEpontosJogadorEMjogo $  pisaalcapaoEMjogo jogoexp


