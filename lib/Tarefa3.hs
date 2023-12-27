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
import Tarefa2
import GHC.Base (undefined)

mapaTeste = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
    [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]



inimigo :: [Personagem]
inimigo = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 0, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (-5,-5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}]

per = Personagem {
   posicao = (5,5),
   tamanho = (10,20),
   ressalta = True,
   tipo = Fantasma,
   vida = 1
}
per2 = Personagem {
   posicao = (2,2),
   tamanho = (3,3),
   ressalta = False,
   tipo = Jogador
}

player :: Personagem
player = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (3,4),
                     direcao = Este,
                     tamanho = (1,1),
                     emEscada = False,
                     ressalta = False,
                     vida = 3,
                     pontos = 0,
                     aplicaDano = (True,0.0)
                     }

gethitbox :: Personagem -> Hitbox 
gethitbox l = ((fst (posicao l) - fst (tamanho l)/2, snd (posicao l) - snd (tamanho l)/2 ),(fst (posicao l) + fst(tamanho l)/2, snd (posicao l) + snd (tamanho l)/2)) 



getdamagehitbox :: Personagem -> Hitbox
getdamagehitbox p 
  | direcao p == Este = getdamagehitboxAuxEste (gethitbox p)
  | direcao p == Oeste = getdamagehitboxAuxOeste (gethitbox p)
  | direcao p == Norte = getdamagehitboxAuxNorte (gethitbox p)
  | direcao p == Sul =  getdamagehitboxAuxSul (gethitbox p)

getdamagehitboxAuxEste :: Hitbox -> Hitbox 
getdamagehitboxAuxEste ((x,y),(x2,y2)) = ((x+(x2-x),y),(x2+(x2-x),y2))

getdamagehitboxAuxOeste :: Hitbox -> Hitbox 
getdamagehitboxAuxOeste ((x,y),(x2,y2)) = ((x-(x2-x),y),(x2-(x2-x),y2))

getdamagehitboxAuxNorte :: Hitbox -> Hitbox 
getdamagehitboxAuxNorte ((x,y),(x2,y2)) = ((x,y+(y2-y)),(x2,y2+(y2-y)))

getdamagehitboxAuxSul :: Hitbox -> Hitbox 
getdamagehitboxAuxSul ((x,y),(x2,y2)) = ((x,y-(y2-y)),(x2,y2-(y2-y)))


inimigomorre :: [Personagem] -> [Personagem]
inimigomorre [] = []
inimigomorre (h:t)  =  inimigomorreAux h : inimigomorre t

inimigomorreAux :: Personagem -> Personagem 
inimigomorreAux p | tipo p == Fantasma && vida p == 0 = p { posicao = xaupersonagem (posicao p)}
                  | otherwise = p 

xaupersonagem :: Posicao -> Posicao
xaupersonagem (x,y) = (x+1000,y+1000) 


gravidadeexiste :: Personagem -> Mapa -> Personagem 
gravidadeexiste p (Mapa _ _ blocos) = undefined 

{-
aplicaGravidade :: Mapa -> Personagem -> Personagem
aplicaGravidade mapa personagem =
  if estaSobrePlataforma mapa personagem
    then personagem -- Não aplicar gravidade se estiver sobre uma plataforma
    else aplicarGravidade personagem

-- Verifica se o personagem está sobre uma plataforma no mapa
estaSobrePlataforma :: Mapa -> Personagem -> Bool
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

-- Valor da aceleração devido à gravidade
gravidade :: Double
gravidade = 0.1

-- Atualiza a posição do personagem com base na velocidade
atualizarPosicao :: Personagem -> Velocidade -> Posicao
atualizarPosicao personagem (vx, vy) =
  let (x, y) = posicao personagem
  in (x + vx, y + vy)
  -}
{-
aplicaGravidade :: Mapa -> Personagem -> Personagem 
aplicaGravidade mp p | blocoNaPosicao  mp p == Just -- = novaPosicao p
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
    else Nothing
    where linha = round y
          coluna = round x 
-}



--perdevidainimigo :: Personagem -> Personagem -> Personagem 
--perdevidainimigo (Fantasma {vida = x}) Jogador = if sobreposicao (gethitbox Fantasma) (getdamagehitbox Jogador) == True 
--                                        then perdevidainimigo Fantasma {vida = x-1} Jogador 
--                                        else perdevidainimigo Fantasma {vida = x} Jogador



recolhecolecionavel :: Personagem -> Colecionavel -> Personagem 
recolhecolecionavel = undefined




--movimenta :: Semente -> Tempo -> Jogo -> Jogo
--movimenta = undefined
