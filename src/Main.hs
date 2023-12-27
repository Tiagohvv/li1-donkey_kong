module Main where
import Tarefa1
import Tarefa3
import Tarefa2
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random


data Estado = Estado 
  { jogo :: Jogo,
    imagens :: Imagens}

type Imagens = [(Imagem, Picture)]
data Imagem = Mario | Pscada | Plataforma 
             deriving (Show, Eq)  

janela :: Display 
janela = InWindow (500,500) (500, 200)

corFundo :: Color 
corFundo = black  

frameRate :: Int 
frameRate = 50 

desenhaMario :: Imagens -> Personagem -> Picture 
desenhaMario imgs (x,y) = Translate x y $ Scale 0.5 0.5 imagem 
   where imagem = getImagem Mario imgs 

desenha :: Estado -> IO Picture 
desenha Estado {jogo = Jogo, imagens = imgs} = return $ Pictures $ desenhaMario imgs 
--reage :: Event -> Estado -> IOEstado 

getImagem :: Imagem -> Imagens -> Picture 
getImagem key dicionario = fromJust $ lookup k d 



main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
  imgs <- loadimages
  playIO janela corFundo frameRate (Estado {jogo = Jogo, imagens = imgs})



loadimages :: IO Imagens 
loadimages = do 
  mario <- loadBMP "mario2.bmp"

  let imgs = [(Mario,mario)]
  return imgs



 


