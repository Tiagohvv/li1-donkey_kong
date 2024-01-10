{-|
Module      : Desenha
Description : Desenha o jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização de LI1 em 2023/24.
-}
module Desenha where
import Tarefa1
    

carregaImagens :: Picture -> Picture IO
carregaImagens = do 
      escada <- loadBMP "escada.bmp"
      fantasma <- loadBMP "fantasma.bmp"
      bloco <- loadBMP "bloco.bmp"
      mario <- loadBMP "mario2.bmp"
      surpresa <- loadBMP "untitled.bmp"


desenhaBloco :: [Bloco] -> Picture
desenhaBloco (h:t) = aux a b h : aux a (b + 1) t
           where aux :: Bloco -> Picture
                 aux a b (x:y) | x == E = Translate (0 + b) + (0 + a) $ Color orange $ rectangleSolid 10 50 : aux (a + 1) b y
                               | x == A = Translate (0 + b) + (0 + a) $ Color orange $ rectangleSolid 10 50 : aux (a + 1) b y
                               | x == P = Translate (0 + b) + (0 + a) $ Color orange $ rectangleSolid 10 50 : aux (a + 1) b y

