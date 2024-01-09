module Main where
import LI12324 
import Tarefa1
import Tarefa3
import Tarefa2
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Data.Maybe
import GHC.Float (double2Float)


data Estado = Estado 
             { jogo :: Jogo,
               imagens :: Imagens }



type Imagens = [(Imagem, Picture)]
data Imagem = Mario | Escadaimg | Plataformaimg | Alcapaoimg  
             deriving (Show, Eq)  


corFundo :: Color 
corFundo = black  

frameRate :: Int 
frameRate = 50 

desenhaLinha :: Float -> Float -> [Bloco] -> Imagens -> [Picture]
desenhaLinha x y (h:t) imgs | h == Plataforma = translate x y (getImagem Plataformaimg imgs) : desenhaLinha x y t imgs  
                            | h == Alcapao = translate x y (getImagem Alcapaoimg imgs) : desenhaLinha x y t imgs 
                            | h == Escada = translate x y (getImagem Escadaimg imgs) : desenhaLinha x y t imgs 
                            | otherwise = desenhaLinha x y t imgs 

l :: Float 
l = 50 

desenhaMapa :: Float -> Float -> Mapa -> Imagens -> [Picture]   
desenhaMapa x y (Mapa (_ ,_) _ (linhabloco : restos)) imgs = linha ++ resto 
          where linha = desenhaLinha x y linhabloco imgs 
                resto = desenhaMapa x (y-l) (Mapa (_,_) _ restos) imgs  




--desenhaMario :: Imagens -> Mapa -> Picture 
--desenhaMario imgs (Mapa (pos, _) _ _) = Translate (double2Float fst pos, double2Float snd pos) $ Scale 0.5 0.5 imagem 
--where imagem = getImagem Mario imgs 




desenha :: Estado -> IO Picture 
desenha estado = return $ Pictures [desenhaMapa x y mapaTeste imgs, desenhaMario imgs mapaTeste ]   


--reage :: Event -> Estado -> IOEstado 

getImagem :: Imagem -> Imagens -> Picture 
getImagem key dicionario = fromJust $ lookup key dicionario 



main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
  imgs <- loadimages
  playIO 
        (InWindow "Game" (500,500) (500, 200))
        corFundo 
        frameRate 
         (Estado {jogo = jogoexp, imagens= imgs}) 
        desenha 



loadimages :: IO Imagens 
loadimages = do 
  mario <- loadBMP "mario2.bmp"
  plataforma <- loadBMP "plataforma.bmp" 
  alcapao <- loadBMP "alcapao.bmp"

  let imgs = [(Mario,mario), (Plataformaimg,plataforma), (Alcapaoimg,alcapao)]
  return imgs



 


