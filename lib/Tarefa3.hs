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

mapaTeste = Mapa ((9,6), Oeste) (0.5, 2.5) matrizJogoExp

matrizJogoExp :: [[Bloco]]
matrizJogoExp =[
     [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]
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
jogoexp = Jogo mapaTeste inimigo colec player  

colec :: [(Colecionavel, Posicao)]
colec = [(Moeda, (2,6)), (Martelo, (2,16))]

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
                    direcao = Oeste}   
                    ,
                    
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (6,8), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este}  
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
                    direcao = Oeste} 
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
                    direcao = Oeste}           
                    ]


player :: Personagem
player = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (6,5),
                     direcao = Este,
                     tamanho = (1,1), 
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


temGravidadeEMjogo :: Tempo -> Jogo -> Jogo 
temGravidadeEMjogo t j = j {jogador = temGravidade t (jogador j) (mapa j) } 

temGravidade :: Tempo -> Personagem -> Mapa -> Personagem                    
temGravidade t p a@(Mapa _ _ matriz) | (blocoNaPosicao a (fst (posicao p), snd (posicao p) +0.5) /= Just Plataforma && blocoNaPosicao a (fst (posicao p), snd (posicao p) +0.5) /= Just Alcapao) && not (estaEmEscada a p) = velocidadeGravidade t p 
                                     | otherwise = p {velocidade = (fst(velocidade p),if (snd(velocidade p) < 0) || (estaEmPlataforma a p && ((snd(velocidade p) > 0) || snd(velocidade p) < 0)) || (estaEscadaEPlat a p && snd(velocidade p) > 0) || (estaEmEscada a p && not (verificaPlataforma a p) && snd(velocidade p) > 0) then snd(velocidade p) else 0)} 


velocidadeGravidade :: Tempo -> Personagem -> Personagem 
velocidadeGravidade t p = p {velocidade = (fst (velocidade p) , (snd (velocidade p)) + ((snd gravidade) * t))}


verificaPlataforma :: Mapa -> Personagem -> Bool 
verificaPlataforma a@(Mapa _ _ blocos) p | blocoNaPosicao a (fst(posicao p), (snd (posicao p))+0.5) == Just Plataforma = True 
                                         | otherwise = False 

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
verificaEscadaAbaixo a@(Mapa _ _ blocos) p | (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1.5) == Just Escada) || (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1)== Just Escada) = True 
                                           | otherwise = False




{-}


*/aplicaGravidade :: Mapa -> Personagem -> Personagem
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


-- Fantasma perde vida ao ser martelado 
perdevidainimigoEMjogo :: Jogo -> Jogo 
perdevidainimigoEMjogo j = j {inimigos = perdevidainimigo (inimigos j) (jogador j) } 

perdevidainimigo :: [Personagem] -> Personagem -> [Personagem] 
perdevidainimigo [] _ = [] 
perdevidainimigo (ini:inis) j  = if ((aplicaDano j == (True, snd (aplicaDano j))) && (sobreposicao (getdamagehitbox j) (gethitbox ini) == True) )
                                 then ini {vida = (vida ini)-1} : perdevidainimigo inis j 
                                 else ini : perdevidainimigo inis j 

   


--jogador perde vida ao ser tocado pelo fantasma          
perdevidaJogadorEMjogo :: Jogo -> Jogo 
perdevidaJogadorEMjogo j = j {jogador = perdevidaJogador (mapa j) (jogador j) (inimigos j) }


perdevidaJogador :: Mapa -> Personagem -> [Personagem] -> Personagem 
perdevidaJogador _ j [] = j                          
perdevidaJogador a@(Mapa (p1,p2) _ _) j (ini:inis) | colisoesPersonagens j ini = perdevidaJogador a (j {vida = (vida j)-1, posicao =p1, direcao = p2}) inis 
                                                 | otherwise = perdevidaJogador a j inis 


{-
jogadorMorreEmJogo :: Jogo -> Jogo 
jogadorMorreEmJogo j = j {jogador = jogadorMorre (mapa j) (jogador j)}

jogadorMorre :: Mapa -> Personagem -> Personagem
jogadorMorre a@(Mapa (p1,p2) _ l) p | vida p == 0 = p{posicao = p1, direcao = p2} 
                                    | otherwise = p 
-}

-- arma o jogador se for martelo e aumenta pontos se for moeda. Desaparecem se forem recolhidos 
armaEpontosJogadorEMjogo :: Jogo -> Jogo 
armaEpontosJogadorEMjogo j = j {colecionaveis = xaucolec (jogador j) (colecionaveis j), jogador = armaEpontosJogador (jogador j) (colecionaveis j)}


xaucolec :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
xaucolec j [] = []
xaucolec j ((col,pos):cols) | sobreposicao (gethitbox j) (gethitboxcol pos) = (col,xaupersonagem pos) : xaucolec j cols 
                            | otherwise = (col,pos) : xaucolec j cols       


armaEpontosJogador :: Personagem -> [(Colecionavel, Posicao)] -> Personagem 
armaEpontosJogador j [] = j 
armaEpontosJogador j (col:cols) | sobreposicao (gethitbox j) (gethitboxcol (snd col))  &&  (fst col == Martelo) = armaEpontosJogador (j {aplicaDano = (True, 10)}) cols
                                | sobreposicao (gethitbox j) (gethitboxcol (snd col)) &&  (fst col == Moeda) = armaEpontosJogador (j {pontos = (pontos j) +1}) cols  
                                | otherwise = armaEpontosJogador j cols 


-- Função que faz um alçapão desaparecer se o jogador o pisar  

pisaalcapaoEMjogo :: Jogo -> Jogo 
pisaalcapaoEMjogo j = j {mapa = pisaalcapaoJogador (jogador j) (0,0) (mapa j)}                         

--pisaalcapao :: Posicao -> Mapa -> Bool 
--pisaalcapao (x, y) a@(Mapa _ _ blocos) = blocoNaPosicao a ( x, y+1) == Just Alcapao 

pisaalcapaoJogador :: Personagem -> Posicao -> Mapa -> Mapa 
pisaalcapaoJogador  p (x,y) a@(Mapa (p1, d) p2 []) = a 
pisaalcapaoJogador  p (x,y) a@(Mapa (p1, d) p2 blocos)  = Mapa (p1, d) p2 (pisaalcapao2 p (x,y) blocos)               

trocarBlocoNaPosicao :: Bloco -> Bloco -> Posicao -> [[Bloco]] -> [[Bloco]]
trocarBlocoNaPosicao _ _ _ [] = []  
trocarBlocoNaPosicao antigo novo (coluna, linha) matriz =
    take (round linha) matriz ++
    [trocarLinhaNaPosicao antigo novo coluna (matriz !! round linha)] ++
    drop ( round linha + 1) matriz
  where
    trocarLinhaNaPosicao _ _ _ [] = []  
    trocarLinhaNaPosicao antigo novo coluna (b:bs)
      | coluna == 0 = novo : bs  
      | otherwise = b : trocarLinhaNaPosicao antigo novo (coluna - 1) bs


pisaalcapao2 :: Personagem -> Posicao -> [[Bloco]] -> [[Bloco]]
pisaalcapao2 _ _ [] = []
pisaalcapao2 p (x,y) (bloco:blocos) = pisaalcapao p (x,y) bloco : pisaalcapao2 p (x,y+1) blocos 

pisaalcapao :: Personagem -> Posicao -> [Bloco] -> [Bloco] 
pisaalcapao _ _ [] = []
pisaalcapao p (x,y) (b1:b) | tipo p == Jogador && b1 == Alcapao && sobreposicao (gethitboxcol (x,y)) (gethitbox p) = (Vazio: (pisaalcapao p (x+1,y) b))
                                     | otherwise = (b1:pisaalcapao p (x+1,y) b)

hitboxAlcapao :: [Posicao] -> [Hitbox] 
hitboxAlcapao [] = [] 
hitboxAlcapao blocos = gethitboxcol (head (blocos)) : hitboxAlcapao (tail blocos) 
                           

tiraposicoesAlca :: Mapa -> [Posicao] 
tiraposicoesAlca a@(Mapa _ _ (blocos)) = posicaoBlocos1 Alcapao blocos 

desapareceAlcapaoJogo :: Jogo -> Jogo
desapareceAlcapaoJogo j = j {mapa = desapareceAlcapao (mapa j) (jogador j) }

aplicaEfeitoAlcapao :: ((Int, Int), Bloco) -> Personagem -> Bloco
aplicaEfeitoAlcapao ((x, y), bloco) p =
  case gethitbox p of
    ((xi, yi), (xs, ys)) ->
      let
        xchao = floor xs
        ychao = floor ys
        xchao2 = floor xi
        ychao2 = floor ys
      in
        if bloco == Alcapao && (y == ychao || y == ychao2) && (x == xchao || x == xchao2)
          then Vazio
          else bloco

desapareceAlcapao :: Mapa -> Personagem -> Mapa
desapareceAlcapao (Mapa (pi,di) pf blocos) p = 
  let posicoesEblocos = zipWith (\x y -> zipWith (\bloco x -> ((x,y), bloco)) x [0..]) blocos [0..]
   in Mapa (pi,di) pf (map (\x -> map (\bloco -> aplicaEfeitoAlcapao bloco p) x) posicoesEblocos)


-- Atualiza as posicoes com a velocidade 
posicaoatualizada :: Velocidade -> Tempo -> Posicao -> Posicao 
posicaoatualizada (v1,v2) t (x,y) = ((x+v1*t),(y+v2*t))  

posicaoatualizadaPer :: Tempo -> Mapa -> Personagem -> Personagem 
posicaoatualizadaPer t a@(Mapa _ _ blocos) p = p {posicao= posicaoatualizada (velocidade p) t (posicao p) } 

posicaoatualizadaIni :: Tempo -> Mapa -> [Personagem] -> [Personagem] 
posicaoatualizadaIni _ _ [] = []
posicaoatualizadaIni t a@(Mapa _ _ blocos) (i1:i2) = (i1 {posicao= posicaoatualizada (velocidade i1) t (posicao i1)}) : posicaoatualizadaIni t a i2 


posicaoatualizadaPerEmjogo :: Tempo -> Jogo -> Jogo 
posicaoatualizadaPerEmjogo t j = j {jogador = posicaoatualizadaPer t (mapa j) (jogador j) , inimigos = posicaoatualizadaIni t (mapa j) (inimigos j) } 

limitesEmJogo :: Jogo -> Jogo 
limitesEmJogo j = j {inimigos = limitesIni (mapa j) (inimigos j), jogador = limites (mapa j) (jogador j) } 

verificaIniO :: Mapa -> Personagem -> Bool 
verificaIniO a@(Mapa _ _ blocos) p | (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1) == Just Plataforma) && (blocoNaPosicao a ((fst(posicao p))-1, (snd (posicao p))+1) == Just Vazio) = True 
                                   | otherwise = False 

verificaIniE :: Mapa -> Personagem -> Bool 
verificaIniE a@(Mapa _ _ blocos) p | (blocoNaPosicao a (fst(posicao p), (snd (posicao p))+1) == Just Plataforma) && (blocoNaPosicao a ((fst(posicao p))+1, (snd (posicao p))+1) == Just Vazio) = True 
                                   | otherwise = False 

limitesIni :: Mapa -> [Personagem] -> [Personagem]
limitesIni _ [] = []
limitesIni a@(Mapa _ _ (bloco:blocos)) (h:t) | ressalta h == True && ((fst (posicao h) >= fromIntegral (length bloco)) || verificaIniE a h) = h {velocidade = (fst (velocidade h)* (-1),snd (velocidade h)), direcao = Oeste } : limitesIni a t
                                             | ressalta h == True && ((fst (posicao h) <= 0) || verificaIniO a h) = h {velocidade = (fst (velocidade h)* (-1),snd (velocidade h)), direcao= Este} : limitesIni a t 
                                             | otherwise = (h:t)

limites :: Mapa -> Personagem -> Personagem 
limites a@(Mapa _ _ (bloco:blocos)) p | (fst (posicao p) >= fromIntegral (length bloco)) = p {posicao = (fromIntegral (length bloco)-1, snd (posicao p))} 
                                      | (fst (posicao p) <= 0) = p {posicao = (0.5, snd (posicao p))} 
                                      | otherwise = p 
                                       

tiraposicoes :: Mapa -> [Double]
tiraposicoes (Mapa (a,b) c []) = [] 
tiraposicoes (Mapa (a,b) c (bloco:blocos)) = fromIntegral (length bloco) : tiraposicoes (Mapa (a,b) c blocos) 

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta x t jogoexp = temGravidadeEMjogo t $ posicaoatualizadaPerEmjogo t $ inimigomorreEMjogo $  pisaalcapaoEMjogo  $ perdevidainimigoEMjogo $ perdevidaJogadorEMjogo $ armaEpontosJogadorEMjogo $ limitesEmJogo $ estaEmEscadaEmJogo  jogoexp


