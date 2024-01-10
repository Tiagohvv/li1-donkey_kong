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

data Estado = Estado 
            { jogo :: Jogo,
              imagens :: Imagens }

type EstadoGloss = (Estado,Picture)


type Imagens = [(Imagem, Picture)]
data Imagem = Mario | Escadaimg | Plataformaimg | Alcapaoimg | Fantasmaimg 
             deriving (Show, Eq)  


corFundo :: Color 
corFundo = black  

frameRate :: Int 
frameRate = 50 

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
estadoexp = Estado {jogo = jogoexp} 

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
desenha estado = return $ Pictures (desenhaMario (imagens estado) mapaTeste : desenhaMapa 0 0 mapaTeste (imagens estado))   


--desenhaEstadoGloss :: EstadoGloss -> Picture 
--desenhaEstadoGloss ((x,y),img) = Translate x y img 

reageTempoGloss :: Float -> Estado -> IO Estado 
reageTempoGloss _ x = return x 

reageEventGloss :: Event -> Estado -> IO Estado 
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
  --mario <- loadBMP "mario2.bmp"
  plataforma <- loadBMP "imagensbmp/plataforma.bmp" 
  alcapao <- loadBMP "imagensbmp/alcapao.bmp"
  escada <- loadBMP "imagensbmp/ladder.bmp" 
  mario <- loadBMP "imagensbmp/Mario1.bmp"

  return estado {imagens = [(Plataformaimg,plataforma), (Alcapaoimg,alcapao), (Escadaimg,escada), (Mario,mario)]}


