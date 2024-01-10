module Main where
import LI12324 
import Tarefa1
import Tarefa3
import Tarefa2
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
data Imagem = Mario | Escadaimg | Plataformaimg | Alcapaoimg | Fantasmaimg| Jogarimg | Sairimg
             deriving (Show, Eq)  


corFundo :: Color 
corFundo = black  

frameRate :: Int 
frameRate = 50 


desenhaOpcJogar :: Float -> Float -> Imagens -> Picture     -- 613 333
desenhaOpcJogar x y imgs = translate x y (getImagem Jogarimg imgs) 

desenhaOpcSair :: Float -> Float -> Imagens -> Picture 
desenhaOpcSair x y imgs = translate x y (getImagem Sairimg imgs)

desenhaLinha :: Float -> Float -> [Bloco] -> Imagens -> [Picture]
desenhaLinha _ _ [] _ = []
desenhaLinha x y (h:t) imgs | h == Plataforma = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Plataformaimg imgs)) : desenhaLinha (x+1) y t imgs  
                            | h == Alcapao = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Alcapaoimg imgs)) : desenhaLinha (x+1) y t imgs 
                            | h == Escada = translate ((x*escala)-920) ((y*escala)+500) (Scale 0.6 0.6 (getImagem Escadaimg imgs)) : desenhaLinha (x+1) y t imgs 
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




desenhaMario :: Imagens -> Mapa -> Picture 
desenhaMario imgs (Mapa (pos, _) _ _) = translate ((double2Float (fst pos)*escala)-920)  ( (double2Float (negate (snd pos))*escala)+500) (Scale 0.6 0.6 imagem)  
                                 where imagem = getImagem Mario imgs


posicoesmapa :: Mapa -> [Posicao]
posicoesmapa (Mapa a b (bloco:resto)) = posicaoBlocoss bloco ++ posicoesmapa (Mapa a b resto)



desenha :: Estado -> IO Picture 
desenha e@Estado {modo= MenuInicial Sair} = return $ Pictures [desenhaOpcJogar (-300) 0 (imagens e), Scale 1.5 1.5 $ desenhaOpcSair (300) 0 (imagens e)]
desenha e@Estado {modo = MenuInicial Jogar} = return $ Pictures [Scale 1.5 1.5 $ desenhaOpcJogar (-300) 0 (imagens e), desenhaOpcSair (300) 0 (imagens e)]
desenha e@Estado {modo = EmJogo} = return $ Pictures (desenhaMario (imagens e) mapaTeste : desenhaMapa 0 0 mapaTeste (imagens e))   


--desenhaEstadoGloss :: EstadoGloss -> Picture 
--desenhaEstadoGloss ((x,y),img) = Translate x y img 

reageTempoGloss :: Float -> Estado -> IO Estado 
reageTempoGloss _ x = return x 

reageEventGloss :: Event -> Estado -> IO Estado 
reageEventGloss (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo} =
  return e {modo = MenuInicial Jogar}
reageEventGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = EmJogo}
reageEventGloss (EventKey (SpecialKey KeyRight) Down _ _) e@Estado {modo= MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
reageEventGloss (EventKey (SpecialKey KeyLeft) Down _ _) e@Estado {modo = MenuInicial Sair} = 
  return e {modo = MenuInicial Jogar} 
reageEventGloss (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} = exitSuccess 
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
  mario <- loadBMP "imagensbmp/Mario1.bmp"
  jogar <- loadBMP "imagensbmp/play.bmp"
  sair <- loadBMP "imagensbmp/sair.bmp"
 

  return estado {imagens = [(Plataformaimg,plataforma), (Alcapaoimg,alcapao), (Escadaimg,escada), (Mario,mario), (Jogarimg,jogar), (Sairimg,sair)]}


