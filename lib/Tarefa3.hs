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

mapaTeste = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) matrizJogoExp

matrizJogoExp :: [[Bloco]]
matrizJogoExp =[
     [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Alcapao, Vazio, Vazio, Vazio, Vazio]
    ,[Escada, Alcapao, Vazio, Vazio, Alcapao, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]
jogoexp :: Jogo 
jogoexp = Jogo mapaTeste inimigo colec player 

colec :: [(Colecionavel, Posicao)]
colec = [(Moeda, (3,1)), (Martelo, (1,1))]

inimigo :: [Personagem]
inimigo = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 0, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1,1), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}   
                    ,
                    
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 2, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (2,4), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este}          
                    ]


player :: Personagem
player = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (1,1),
                     direcao = Este,
                     tamanho = (2,2),
                     emEscada = False,
                     ressalta = False,
                     vida = 3,
                     pontos = 0,
                     aplicaDano = (False,0.0)
                     }

gethitbox :: Personagem -> Hitbox 
gethitbox l = ((fst (posicao l) - fst (tamanho l)/2, snd (posicao l) - snd (tamanho l)/2 ),(fst (posicao l) + fst(tamanho l)/2, snd (posicao l) + snd (tamanho l)/2)) 

gethitboxcol :: Posicao -> Hitbox 
gethitboxcol l = ((fst l - fst tamanho/2, snd l - snd tamanho/2) ,(fst l + fst tamanho /2, snd l + snd tamanho/2))
                      where tamanho = (1,1)

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

-- inimigo morre quando a vida chega a 0 
inimigomorreEMjogo :: Jogo -> Jogo 
inimigomorreEMjogo j = j {inimigos = inimigomorre (inimigos j)}


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



-- Fantasma perde vida ao ser martelado 
perdevidainimigoEMjogo :: Jogo -> Jogo 
perdevidainimigoEMjogo j = j {inimigos = perdevidainimigo (inimigos j) (jogador j) } 

perdevidainimigo :: [Personagem] -> Personagem -> [Personagem] 
perdevidainimigo [] _ = [] 
perdevidainimigo (ini:inis) j  = if ((aplicaDano j == (True, snd (aplicaDano j))) && (sobreposicao (gethitbox ini) (getdamagehitbox j) == True) )
                                 then ini {vida = (vida ini)-1} : perdevidainimigo inis j 
                                 else ini : perdevidainimigo inis j 

   


--jogador perde vida ao ser tocado pelo fantasma 
perdevidaJogadorEMjogo :: Jogo -> Jogo 
perdevidaJogadorEMjogo j = j {jogador = perdevidaJogador (jogador j) (inimigos j) }



perdevidaJogador :: Personagem -> [Personagem] -> Personagem 
perdevidaJogador j [] = j 
perdevidaJogador j (ini:inis) | colisoesPersonagens j ini = perdevidaJogador (j {vida = (vida j)-1}) inis 
                              | otherwise = perdevidaJogador j inis 


--recolhecolec :: Personagem -> [Colecionavel] -> Colecionavel 
--recolhecolec j col | 


-- arma o jogador se for martelo e aumenta pontos se for moeda (ainda não desaparece)
armaEpontosJogadorEMjogo :: Jogo -> Jogo 
armaEpontosJogadorEMjogo = undefined 

{-
xaucolec :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
xaucolec j [] = []
xaucolec j (col:cols) | sobreposicao (gethitbox j) (gethitboxcol (snd col)) = (xaupersonagem (snd col)) : xaucolec j cols 
                      | otherwise = col : xaucolec j cols 

-}
armaEpontosJogador :: Personagem -> [(Colecionavel, Posicao)] -> Personagem 
armaEpontosJogador j [] = j 
armaEpontosJogador j (col:cols) | sobreposicao (gethitbox j) (gethitboxcol (snd col))  &&  (fst col == Martelo) = armaEpontosJogador (j {aplicaDano = (True, 10)}) cols
                                | sobreposicao (gethitbox j) (gethitboxcol (snd col)) &&  (fst col == Moeda) = armaEpontosJogador (j {pontos = (pontos j) +1}) cols  
                                | otherwise = armaEpontosJogador j cols 





pisaalcapao :: Posicao -> Mapa -> Bool 
pisaalcapao (x,y) a@(Mapa _ _ blocos) =  blocoNaPosicao a (x,y+1) == Just Alcapao 



pisaalcapaoJogador :: Personagem -> Mapa -> Mapa
pisaalcapaoJogador j m@(Mapa (p1,d) p2 []) = m
pisaalcapaoJogador j a@(Mapa (p1,d) p2 (linha1:resto)) | tipo j == Jogador && pisaalcapao (posicao j) a  = Mapa (p1,d) p2 (trocarBlocoNaPosicao Alcapao Vazio (fst (posicao j), snd (posicao j) +1) (linha1:resto))
                                                       | otherwise = a

trocarBlocoNaPosicao :: Bloco -> Bloco -> Posicao -> [[Bloco]] -> [[Bloco]]
trocarBlocoNaPosicao _ _ _ [] = []  -- Se a matriz estiver vazia, não há nada a fazer
trocarBlocoNaPosicao antigo novo (coluna, linha) matriz =
    take (round linha) matriz ++
    [trocarLinhaNaPosicao antigo novo coluna (matriz !! round linha)] ++
    drop (round linha + 1) matriz
  where
    trocarLinhaNaPosicao _ _ _ [] = []  -- Se a linha estiver vazia, não há nada a fazer
    trocarLinhaNaPosicao antigo novo coluna (b:bs)
      | coluna == 0 = novo : bs  -- Substitui o bloco na posição específica
      | otherwise = b : trocarLinhaNaPosicao antigo novo (coluna - 1) bs  -- Mantém o bloco original







trocarBlocoMatriz :: Bloco -> Bloco -> [[Bloco]] -> [[Bloco]]
trocarBlocoMatriz _ _ [] = []  -- Se a matriz estiver vazia, não há nada a fazer
trocarBlocoMatriz antigo novo (linha:resto) =
  trocarLinha antigo novo linha : trocarBlocoMatriz antigo novo resto
  where
    trocarLinha _ _ [] = []  -- Se a linha estiver vazia, não há nada a fazer
    trocarLinha antigo novo (b:bs)
      | b == antigo = novo : trocarLinha antigo novo bs  -- Substitui o bloco antigo pelo novo
      | otherwise = b : trocarLinha antigo novo bs  -- Mantém o bloco original


novaMatrizaux :: [[Bloco]] -> [[Bloco]] 
novaMatrizaux [[]] = [[]]
novaMatrizaux [] = []
novaMatrizaux (linha1:restantes) = substituirBloco Alcapao Vazio linha1 : novaMatrizaux restantes


--substitui blocos
substituirBloco :: Eq a => a -> a -> [a] -> [a]
substituirBloco blocoantes blocodepois = map (\x -> if x == blocoantes then blocodepois else x)
--movimenta :: Semente -> Tempo -> Jogo -> Jogo
--movimenta = undefined
