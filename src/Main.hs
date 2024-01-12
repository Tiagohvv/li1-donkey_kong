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
data Imagem = Mario | MarioO | Escadaimg | Plataformaimg | Alcapaoimg | Vazioimg | Fantasmaimg| Jogarimg | Sairimg | Marteloimg | Moedaimg | Fundo 
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


desenhaColec :: Imagens -> [(Colecionavel, Posicao)] -> [Picture]
desenhaColec _ [] = []
desenhaColec imgs ((col,pos):cols) | col == Martelo = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 0.6 0.6 (getImagem Marteloimg imgs)) : desenhaColec imgs cols 
                                   | col == Moeda = translate ((double2Float (fst pos) *escala)-920) ((double2Float (negate (snd pos))*escala)+500) (Scale 0.6 0.6 (getImagem Moedaimg imgs)) : desenhaColec imgs cols
                                   | otherwise = desenhaColec imgs cols 


desenhaFantasma :: Imagens -> [Personagem] -> [Picture]
desenhaFantasma _ [] = []
desenhaFantasma imgs (h:t) = translate ((double2Float (fst (posicao h))*escala)-920)  ( (double2Float (negate (snd (posicao h)))*escala)+500) (Scale 0.6 0.6 (getImagem Fantasmaimg imgs)) : desenhaFantasma imgs t                         

desenhaMario :: Imagens -> Personagem -> Picture 
desenhaMario imgs p | (direcao p)== Este =  translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 0.6 0.6 imagem)  
                    | otherwise = translate ((double2Float (fst (posicao p))*escala)-920)  ( (double2Float (negate (snd (posicao p)))*escala)+500) (Scale 0.6 0.6 imagem2)            
                                 where imagem = getImagem Mario imgs
                                       imagem2 = getImagem MarioO imgs


desenhaHitbox :: Personagem -> Color -> Picture 
desenhaHitbox l cor = Color cor $ line [(double2Float (fst (posicao l)) - double2Float (fst (tamanho l)/2), double2Float (snd (posicao l)) + double2Float (snd (tamanho l)/2) ),(double2Float (fst (posicao l)) - double2Float (fst (tamanho l)/2), double2Float (snd (posicao l)) - double2Float (snd (tamanho l)/2) ), (double2Float (fst (posicao l)) + double2Float (fst(tamanho l)/2), double2Float (snd (posicao l)) - double2Float (snd (tamanho l)/2)), (double2Float (fst (posicao l)) + double2Float (fst(tamanho l)/2), double2Float (snd (posicao l)) + double2Float (snd (tamanho l)/2))]



posicoesmapa :: Mapa -> [Posicao]
posicoesmapa (Mapa a b (bloco:resto)) = posicaoBlocoss bloco ++ posicoesmapa (Mapa a b resto)



desenha :: Estado -> IO Picture 
desenha e@Estado {modo= MenuInicial Sair} = return $ Pictures ([translate 0 0 (getImagem Fundo (imagens e))]++[desenhaOpcJogar (-300) 0 (imagens e), desenhaOpcSairSelec (300) 0 (imagens e)] )
desenha e@Estado {modo = MenuInicial Jogar} = return $ Pictures ([translate 0 0 (getImagem Fundo (imagens e))] ++[desenhaOpcJogarSelec (-300) 0 (imagens e), desenhaOpcSair (300) 0 (imagens e)])
desenha e@Estado {modo = EmJogo} = return $ Pictures ((desenhaMapa 0 0 (sacaMapa (jogo e)) (imagens e))++ [desenhaMario (imagens e) (sacaJogador (jogo e))]++ (desenhaFantasma (imagens e) (inimigos (jogo e))) ++ (desenhaColec (imagens e) (sacaCol (jogo e)))++ [translate ((double2Float (fst (posicao (jogador (jogo e))))*escala)-920)  ( (double2Float (negate (snd (posicao (jogador (jogo e)))))*escala)+500) (Scale 0.6 0.6 (desenhaHitbox (jogador (jogo e)) green))])
   


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



reageTempoGloss :: Float -> Estado -> IO Estado 
reageTempoGloss t estado = 
  return estado {jogo = jogoAtualizado} 
                         where jogoAtualizado = movimenta semente tempo (jogo estado)  

reageEventGloss :: Event -> Estado -> IO Estado 
reageEventGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = EmJogo}
reageEventGloss (EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo= MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
reageEventGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = MenuInicial Sair} = 
  return e {modo = MenuInicial Jogar} 
reageEventGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} = exitSuccess

reageEventGloss (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo} =
  return e {modo = MenuInicial Jogar}
reageEventGloss (EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just AndarDireita) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyRight) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Parar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just AndarEsquerda) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyLeft) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Parar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyUp) Down _ _)  e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Subir) (jogo e)} 
reageEventGloss (EventKey (SpecialKey KeyUp) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Parar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeySpace) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Saltar) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Descer) (jogo e)}
reageEventGloss (EventKey (SpecialKey KeyDown) Up _ _) e@Estado {modo = EmJogo} =
  return e {jogo = atualiza [Nothing,Nothing] (Just Parar) (jogo e)}  
reageEventGloss _ e = return e







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
  fantasma <- loadBMP "imagensbmp/fantasma.bmp"
  jogar <- loadBMP "imagensbmp/play(1).bmp"
  sair <- loadBMP "imagensbmp/sair.bmp"
  martelo <- loadBMP "imagensbmp/martelo.bmp"
  moeda <- loadBMP "imagensbmp/moeda.bmp"
  fundo <- loadBMP "imagensbmp/fundo.bmp"

 

  return estado {imagens = [(Plataformaimg,plataforma), (Alcapaoimg,alcapao), (Escadaimg,escada), (Mario,mario), (Jogarimg,jogar), (Sairimg,sair), (MarioO,marioO), (Vazioimg,vazio), (Marteloimg,martelo),(Moedaimg,moeda),(Fantasmaimg,fantasma), (Fundo,fundo)]}


