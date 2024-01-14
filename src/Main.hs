module Main where
import LI12324 
import Tarefa1
import Tarefa3
import Tarefa2
import Tarefa4
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game 

import System.Random
import Data.Maybe ( fromJust )
import GHC.Float (double2Float)
import System.Exit (exitSuccess)

data Estado = Estado 
            { jogo :: Jogo,
              imagens :: Imagens,
              modo :: Modo  
              }

data Modo = MenuInicial OpcoesMenu | EmJogo  deriving (Show,Eq)

data OpcoesMenu = Jogar | Opcoes | Sair   deriving (Show,Eq)        

type EstadoGloss = (Estado,Picture)


type Imagens = [(Imagem, Picture)]
data Imagem = Mario | MarioO | MarioMart | MarioMartCon | MarioCai | MarioEscada | Escadaimg | Plataformaimg | Alcapaoimg | Vazioimg | Fantasmaimg | FantasmaContimg | Macaco| Jogarimg | Sairimg | Marteloimg | Moedaimg | Fundo | Vidaimg | Chaveimg | Portaimg | GameOver | N0 | N1 | N2 | N3 | N4 | N5 | Score | Estrelaimg | FundoE | Pausaimg
             deriving (Show, Eq)  


corFundo :: Color 
corFundo = black  

frameRate :: Int 
frameRate = 50 


desenhaOpcJogar :: Float -> Float -> Imagens -> Picture     -- 613 333
desenhaOpcJogar x y imgs = translate x y (Scale 0.6 0.6 (getImagem Jogarimg imgs)) 

desenhaOpcJogarSelec :: Float -> Float -> Imagens -> Picture     -- 613 333
desenhaOpcJogarSelec x y imgs = translate x y (Scale 1.5 1.5 (getImagem Jogarimg imgs))

desenhaOpcSair :: Float -> Float -> Imagens -> Picture 
desenhaOpcSair x y imgs = translate x y (Scale 0.6 0.6 (getImagem Sairimg imgs))

desenhaOpcSairSelec :: Float -> Float -> Imagens -> Picture 
desenhaOpcSairSelec x y imgs = translate x y (Scale 1.5 1.5 (getImagem Sairimg imgs))



desenhaLinha :: Float -> Float -> [Bloco] -> Imagens -> [Picture]
desenhaLinha _ _ [] _ = []
desenhaLinha x y (h:t) imgs | h == Plataforma = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Plataformaimg imgs)) : desenhaLinha (x+1) y t imgs  
                            | h == Alcapao = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Alcapaoimg imgs)) : desenhaLinha (x+1) y t imgs 
                            | h == Escada = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Escadaimg imgs)) : desenhaLinha (x+1) y t imgs 
                            | h == Vazio = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Vazioimg imgs)) : desenhaLinha (x+1) y t imgs 
                            | otherwise = desenhaLinha (x+1) y t imgs 

escala :: Float 
escala = 60 

l :: Float 
l = 1 

estadoexp :: Estado 
estadoexp = Estado {jogo = jogoexp, modo = MenuInicial Jogar} 

--estadoGlossInicial :: Picture -> EstadoGloss 
--estadoGlossInicial a = (estadoinicial, z)



desenhaMapa :: Float -> Float -> Mapa -> Imagens -> [Picture]   
desenhaMapa _ _ (Mapa _ _ []) _ = [] 
desenhaMapa x y (Mapa (a ,b) c (linhabloco : restos)) imgs = linha ++ resto 
          where linha = desenhaLinha x y linhabloco imgs 
                resto = desenhaMapa x (y-l) (Mapa (a,b) c restos) imgs  

desenhaVidas :: Float -> Float -> Imagens -> Picture 
desenhaVidas x y imgs = translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem Vidaimg imgs)) 

desenhaScore1 :: Float -> Float -> Imagens -> Picture 
desenhaScore1 x y imgs = translate ((x *escala)-920) ((y *escala)+500) (Scale 3 3 (getImagem Score imgs))

desenhaScore :: Float -> Float -> Personagem -> Imagens -> Picture 
desenhaScore x y p imgs | pontos p == 0 = translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem N0 imgs)) 
                        | pontos p == 1 =translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem N1 imgs))
                        | pontos p == 2 =translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem N2 imgs))
                        | pontos p == 3 =translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem N3 imgs))
                        | pontos p == 4 =translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem N4 imgs))
                        | otherwise = translate ((x *escala)-920) ((y *escala)+500) (Scale 2 2 (getImagem N5 imgs))

desenhaChave :: Float -> Float -> Imagens -> Personagem -> Picture 
desenhaChave x y imgs p = translate ((x *escala)-920) ((y *escala)+500) (Scale 0.05 0.05 (getImagem Chaveimg imgs))


desenhaEstrela :: Mapa -> Imagens -> Picture 
desenhaEstrela (Mapa _ (x,y) _) imgs = translate ((double2Float x *escala)-920) ((double2Float (negate y) *escala)+500) (Scale 0.1 0.1 (getImagem Estrelaimg imgs))


desenhaColec :: Imagens -> [(Colecionavel, Posicao)] -> [Picture]
desenhaColec _ [] = []
desenhaColec imgs ((col,pos):cols) | col == Martelo = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 2.7 2.7 (getImagem Marteloimg imgs)) : desenhaColec imgs cols 
                                   | col == Moeda = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 0.6 0.6 (getImagem Moedaimg imgs)) : desenhaColec imgs cols
                                   | col == Vida = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 2 2 (getImagem Vidaimg imgs)) : desenhaColec imgs cols
                                   | col == Chave = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 0.1 0.1 (getImagem Chaveimg imgs)) : desenhaColec imgs cols
                                   | col == Porta = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 1 6.7 (getImagem Portaimg imgs)) : desenhaColec imgs cols
                                   | otherwise = desenhaColec imgs cols 


desenhaFantasma :: Imagens -> [Personagem] -> [Picture]
desenhaFantasma _ [] = []
desenhaFantasma imgs (h:t) | tipo h == MacacoMalvado = translate ((double2Float (fst (posicao h))*escala)-920)  ( (double2Float (negate (snd (posicao h)))*escala)+500) (Scale 5 5 (getImagem Macaco imgs)) : desenhaFantasma imgs t   
                           | tipo h == Fantasma && direcao h == Oeste = translate ((double2Float (fst (posicao h))*escala)-920)  ( (double2Float (negate (snd (posicao h)))*escala)+500) (Scale 0.6 0.6 (getImagem Fantasmaimg imgs)) : desenhaFantasma imgs t                      
                           | otherwise = translate ((double2Float (fst (posicao h))*escala)-920)  ( (double2Float (negate (snd (posicao h)))*escala)+500) (Scale 0.6 0.6 (getImagem FantasmaContimg imgs)) : desenhaFantasma imgs t
                



desenhaMario :: Imagens -> Personagem -> Picture 
desenhaMario imgs p | (snd (velocidade p) > 0) && (emEscada p == False) = translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 2.7 2.5 imagem5)
                    | (emEscada p) && (fst (velocidade p))==0  =  translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 2.7 2.5 imagem6)
                    | ((direcao p)==Este) && (fst (aplicaDano p) == True ) && (snd (aplicaDano p)>0 ) = translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 2.7 2.5 imagem3)  
                    | ((direcao p)==Oeste) && (fst (aplicaDano p) == True ) && (snd (aplicaDano p)>0 ) = translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 2.7 2.5 imagem4)                      
                    | (direcao p)== Este && (snd (velocidade p) <= 0) =  translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 2.7 2.5 imagem)   
                    | otherwise =  translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 2.7 2.5 imagem2)  
                                 where imagem = getImagem Mario imgs
                                       imagem2 = getImagem MarioO imgs
                                       imagem3 = getImagem MarioMart imgs
                                       imagem4 = getImagem MarioMartCon imgs 
                                       imagem5 = getImagem MarioCai imgs
                                       imagem6 = getImagem MarioEscada imgs 


desenhaHitbox :: Personagem -> Color -> Picture 
desenhaHitbox l cor = Color cor $ line [(double2Float (fst (posicao l)) - double2Float (fst (tamanho l)/2), double2Float (snd (posicao l)) + double2Float (snd (tamanho l)/2) ),(double2Float (fst (posicao l)) - double2Float (fst (tamanho l)/2), double2Float (snd (posicao l)) - double2Float (snd (tamanho l)/2) ), (double2Float (fst (posicao l)) + double2Float (fst(tamanho l)/2), double2Float (snd (posicao l)) - double2Float (snd (tamanho l)/2)), (double2Float (fst (posicao l)) + double2Float (fst(tamanho l)/2), double2Float (snd (posicao l)) + double2Float (snd (tamanho l)/2))]



posicoesmapa :: Mapa -> [Posicao]
posicoesmapa (Mapa a b (bloco:resto)) = posicaoBlocoss bloco ++ posicoesmapa (Mapa a b resto)



desenha :: Estado -> IO Picture 
desenha e@Estado {modo= MenuInicial Sair} = return $ Pictures ([translate 0 0 (getImagem Fundo (imagens e))]++[desenhaOpcJogar (-300) 0 (imagens e), desenhaOpcSairSelec (300) 0 (imagens e)] )
desenha e@Estado {modo = MenuInicial Jogar} = return $ Pictures ([translate 0 0 (getImagem Fundo (imagens e))] ++[desenhaOpcJogarSelec (-300) 0 (imagens e), desenhaOpcSair (300) 0 (imagens e)])   
desenha e@Estado {modo= EmJogo} | pausa (jogo e) = return $ translate 0 0 (Scale 9 9 (getImagem Pausaimg (imagens e)))
                                | sobreposicao (gethitbox (jogador (jogo e))) (gethitboxcol (13,3)) = return $ translate 0 0 (Scale 1 1 (getImagem FundoE (imagens e)))
                                | temChave (jogador (jogo e)) && vida (jogador (jogo e)) == 4 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaChave 27 0 (imagens e) (jogador (jogo e))] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ [desenhaVidas 1 0 (imagens e)] ++ [desenhaVidas 2 0 (imagens e)] ++ [desenhaVidas 3 0 (imagens e)] ++(desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e)))) 
                                | temChave (jogador (jogo e)) && vida (jogador (jogo e)) == 3 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaChave 27 0 (imagens e) (jogador (jogo e))] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ [desenhaVidas 1 0 (imagens e)] ++ [desenhaVidas 2 0 (imagens e)] ++ (desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e)))) 
                                | temChave (jogador (jogo e)) && vida (jogador (jogo e)) == 2 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaChave 27 0 (imagens e) (jogador (jogo e))] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ [desenhaVidas 1 0 (imagens e)] ++ (desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e))))
                                | temChave (jogador (jogo e)) && vida (jogador (jogo e)) == 1 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaChave 27 0 (imagens e) (jogador (jogo e))] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ (desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e))))
                                | (not (temChave (jogador (jogo e)))) && vida (jogador (jogo e)) == 4 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ [desenhaVidas 1 0 (imagens e)] ++ [desenhaVidas 2 0 (imagens e)] ++ [desenhaVidas 3 0 (imagens e)] ++(desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e)))) 
                                | (not (temChave (jogador (jogo e)))) &&  vida (jogador (jogo e)) == 3 = return $ Pictures([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ [desenhaVidas 1 0 (imagens e)] ++ [desenhaVidas 2 0 (imagens e)] ++ (desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e)))) 
                                | (not (temChave (jogador (jogo e)))) && vida (jogador (jogo e)) == 2 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ [desenhaVidas 1 0 (imagens e)] ++ (desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e))))
                                | (not (temChave (jogador (jogo e)))) && vida (jogador (jogo e)) == 1 = return $ Pictures ([desenhaEstrela (mapa (jogo e)) (imagens e)] ++[desenhaScore1 29 0 (imagens e)]++ [desenhaScore 30.5 0 (jogador (jogo e)) (imagens e)]++[desenhaVidas 0 0 (imagens e)] ++ (desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e))))
                                | otherwise = return $ translate 0 0 (Scale 9 9 (getImagem GameOver (imagens e)))  
                                
                                

 
sacaCol :: Jogo -> [(Colecionavel, Posicao)] 
sacaCol j = colecionaveis j 

sacaMapa :: Jogo -> Mapa 
sacaMapa j = mapa j 

sacaJogador :: Jogo -> Personagem 
sacaJogador j = jogador j 


--desenhaEstadoGloss :: EstadoGloss -> Picture 
--desenhaEstadoGloss ((x,y),img) = Translate x y img 
tempo :: Tempo 
tempo = 1/50

semente :: Int 
semente = 647484940



  

reageEventGloss :: Event -> Estado -> IO Estado 
reageEventGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = EmJogo}
reageEventGloss (EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo= MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
reageEventGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = MenuInicial Sair} = 
  return e {modo = MenuInicial Jogar} 
reageEventGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} = exitSuccess


reageEventGloss (EventKey (Char 'p') Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = (jogo e) {pausa = not (pausa (jogo e))}}
reageEventGloss (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo} =
  return e {modo = MenuInicial Jogar}
reageEventGloss (EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just AndarDireita) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyRight) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Parar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just AndarEsquerda) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyLeft) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Parar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyUp) Down _ _)  e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Subir) (jogo e)} 
reageEventGloss (EventKey (SpecialKey KeyUp) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Parar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeySpace) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Saltar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Descer) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyDown) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing,Nothing] (Just Parar) (jogo e)}  
reageEventGloss _ e = return e


reageTempoGloss :: Float -> Estado -> IO Estado 
reageTempoGloss t estado = 
  return estado {jogo = jogoAtualizado} 
                         where jogoAtualizado = movimenta semente tempo (jogo estado)


getImagem :: Imagem -> Imagens -> Picture 
getImagem key dicionario = case lookup key dicionario of
    Just img -> img
    Nothing  -> error $ "Imagem não encontrada para a chave: " ++ show key


janela :: Display 
janela = FullScreen 

main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
  estadoinicial <- loadimages estadoexp 
  playIO 
        janela  
        corFundo 
        frameRate 
        estadoinicial 
        desenha   
        reageEventGloss
        reageTempoGloss




loadimages :: Estado -> IO Estado
loadimages estado = do 
  plataforma <- loadBMP "imagensbmp/plataforma.bmp" 
  alcapao <- loadBMP "imagensbmp/alcapao.bmp"
  escada <- loadBMP "imagensbmp/ladder.bmp" 
  vazio <- loadBMP "imagensbmp/Vazio.bmp"
  mario <- loadBMP "imagensbmp/Mario1.bmp"
  marioO <- loadBMP "imagensbmp/MarioContrario.bmp"
  marioMart <- loadBMP "imagensbmp/MarioMartelo.bmp"
  marioMartCon <- loadBMP "imagensbmp/MarioMarteloContrario.bmp"
  marioCai <- loadBMP "imagensbmp/MarioCai.bmp"
  marioEscada <- loadBMP "imagensbmp/marioEscada.bmp"
  fantasma <- loadBMP "imagensbmp/fantasma.bmp"
  fantasmaCont <- loadBMP "imagensbmp/fantasmaContrario.bmp"
  macaco <- loadBMP "imagensbmp/macaco.bmp"
  jogar <- loadBMP "imagensbmp/play(1).bmp"
  sair <- loadBMP "imagensbmp/sair.bmp"
  martelo <- loadBMP "imagensbmp/martelo2.bmp"
  moeda <- loadBMP "imagensbmp/moeda.bmp"
  fundo <- loadBMP "imagensbmp/fundo.bmp"
  vida <- loadBMP "imagensbmp/vida.bmp" 
  chave <- loadBMP "imagensbmp/chave.bmp"
  porta <- loadBMP "imagensbmp/porta.bmp"
  gameover <- loadBMP "imagensbmp/gameover.bmp"
  n0 <- loadBMP "imagensbmp/pontos0.bmp"
  n1 <- loadBMP "imagensbmp/pontos1.bmp"
  n2 <- loadBMP "imagensbmp/pontos2.bmp"
  n3 <- loadBMP "imagensbmp/pontos3.bmp"
  n4 <- loadBMP "imagensbmp/pontos4.bmp"
  n5 <- loadBMP "imagensbmp/pontos5.bmp" 
  score <- loadBMP "imagensbmp/score.bmp"
  estrela <- loadBMP "imagensbmp/estrela.bmp"
  fundoE <- loadBMP "imagensbmp/fundoestrela.bmp"
  pausa <- loadBMP "imagensbmp/pause.bmp"

 

  return estado {imagens = [(Plataformaimg,plataforma), (Alcapaoimg,alcapao), (Escadaimg,escada), (Mario,mario), (Jogarimg,jogar), (Sairimg,sair), (MarioO,marioO), (Vazioimg,vazio), (Marteloimg,martelo),(Moedaimg,moeda),(Fantasmaimg,fantasma), (Fundo,fundo), (MarioMart,marioMart), (MarioMartCon, marioMartCon), (MarioCai,marioCai), (FantasmaContimg,fantasmaCont),(Macaco,macaco), (Vidaimg,vida), (GameOver,gameover), (N0,n0), (N1,n1), (N2,n2),(N3,n3),(N4,n4),(N5,n5), (Score,score), (MarioEscada,marioEscada),(Chaveimg,chave),(Portaimg,porta),(Estrelaimg,estrela), (FundoE,fundoE), (Pausaimg,pausa)]}


