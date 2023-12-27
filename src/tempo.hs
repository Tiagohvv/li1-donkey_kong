{-|
Module      : Tempo
Description : Tempo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização de LI1 em 2023/24.
-}
module Tempo where
jogaFacil :: Event -> Acao -> IO Acao
jogaFacil (EventKey (SpecialKey KeyRight) Down _ _) = return $ Just AndarDireita
jogaFacil (EventKey (SpecialKey KeyRight) Up _ _) = return $ Just Parar
jogaFacil (EventKey (SpecialKey KeyLeft) Down _ _) = return $ Just AndarEsquerda
jogaFacil (EventKey (SpecialKey KeyLeft) Up _ _) = return $ Just Parar
jogaFacil (EventKey (SpecialKey KeyUp) Down _ _) = return $ Just Subir
jogaFacil (EventKey (SpecialKey KeyUp) Up _ _) = return $ Just Parar
jogaFacil (EventKey (SpecialKey KeyDown) Down _ _) = return $ Just Descer
jogaFacil (EventKey (SpecialKey KeyDown) Up _ _) = return $ Just Parar
jogaFacil (EventKey (SpecialKey KeySpace) Down _ _) = return $ Just Saltar