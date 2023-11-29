{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza = undefined


reage :: Event -> Estado -> IO Estado
reage (EventKey (SpecialKey KeyUp) Down _ _) (Jogo (Cobra orientacao posicoes) maca, imgs) = return $ (Jogo (Cobra Norte posicoes) maca, imgs)
reage (EventKey (SpecialKey KeyDown) Down _ _) (Jogo (Cobra orientacao posicoes) maca, imgs) = return $ (Jogo (Cobra Sul posicoes) maca, imgs)
reage (EventKey (SpecialKey KeyLeft) Down _ _) (Jogo (Cobra orientacao posicoes) maca, imgs) = return $ (Jogo (Cobra Oeste posicoes) maca, imgs)
reage (EventKey (SpecialKey KeyRight) Down _ _) (Jogo (Cobra orientacao posicoes) maca, imgs) = return $ (Jogo (Cobra Este posicoes) maca, imgs)
reage _ e = return e
