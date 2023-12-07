{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa5 where

import Data.Maybe
import LI12324
import Data.Bool (Bool)


janela :: Display
janela = InWindow "Donkey Kong" (600, 600) (300, 300)

corFundo :: Color
corFundo = Black

frameRate :: Int
frameRate = 60

convertePosicoes :: Posicao -> Posicao
convertePosicoes (a, b) = (a*10, b*10)

mapaTeste = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
    [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]



desenhaBloco :: [Bloco] -> Picture
desenhaBloco (h:t) = aux a b h : aux a (b + 1) t
           where aux :: Bloco -> Picture
                 aux a b (x:y) | x == E = Translate (0 + b) + (0 + a) $ Color orange $ rectangleSolid 10 50 : aux (a + 1) b y
                               | x == A = Translate (0 + b) + (0 + a) $ Color orange $ rectangleSolid 10 50 : aux (a + 1) b y
                               | x == P = Translate (0 + b) + (0 + a) $ Color orange $ rectangleSolid 10 50 : aux (a + 1) b y



desenhaMario :: Imagens -> Bloco -> Picture
desenhaMario = | c == Oeste = Translate a b $ imagem1
               | c == Este = Translate a b $ imagem
               where imagem

desenhaEstrela = Translate f d $ 

carregarImagens :: IO Imagens
carregarImagens = do
   dmario <- loadBMP "apple.bmp"
   emario <- loadBMP "head_up.bmp"
   return $ [(Apple, apple), (Snake, head_up)]




main :: IO ()
main = do
    putStrLn "Bem-vindo ao Donkey Kong em Haskell!"
    let mapaInicial = Mapa (Coordenada 1 9)
                    [ [Plataforma, Plataforma, Alcapao]
                    , [Escada, Plataforma, Plataforma]
                    , [Vazio, Vazio, Plataforma]
                    , [Plataforma, Plataforma, Plataforma]
                    , [Plataforma, Vazio, Vazio]
                    , [Plataforma, Plataforma, Plataforma]
                    , [Vazio, Vazio, Escada]
                    , [Plataforma, Plataforma, Plataforma]
                    , [Plataforma, Plataforma, Plataforma]
                    , [Vazio, Alcapao, Vazio]
                    ]
        mundoInicial = Mundo (Coordenada 1 9) [Coordenada 2 9, Coordenada 9 8, Coordenada 1 0, Coordenada 5 0] (Coordenada 0 0) False
    gameLoop mundoInicial mapaInicial   

main = play janela corFundo frameRate mapaInicial desenhaBloco