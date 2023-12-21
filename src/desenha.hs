{-|
Module      : Desenha
Description : Desenha o jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização de LI1 em 2023/24.
-}
module Desenha where

carregaImagens :: Picture -> Picture IO
carregaImagens = do 
      escada <- loadBMP "escada.bmp"
      fantasma <- loadBMP "fantasma.bmp"
      bloco <- loadBMP "bloco.bmp"
      mario <- loadBMP "mario2.bmp"
      surpresa <- loadBMP "untitled.bmp"


      