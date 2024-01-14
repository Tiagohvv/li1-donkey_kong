{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : José Miguel Torrinha Paredes Pinho Sampaio <a106908@alunos.uminho.pt>
              Tiago Filipe Sousa Rodrigues Faria Alves <a106883@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where
import LI12324
import Data.Fixed (E0)
import GHC.OldList (elemIndices, elemIndex)
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float)



colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa (_, _) _ mapaTeste) p    | (fst (posicao p) >= fromIntegral (length (head mapaTeste))) = True 
                                              | (fst (posicao p) <= 0) = True
                                              | colisao (criaHitbox p) (espacoBloco (posicaoBlocos (0.0,0.0) (mapaTeste))) == True = True
                                              | otherwise = False




colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens per1 per2 | sobreposicao (criaHitbox per1) (criaHitbox per2) == True = True
                              | otherwise = False

                                           

{-| Função que testa se duas Hitboxs estão a colidir.

== Exemplos

>>> sobreposicao ((1,1),(2,2)) ((1.5,1.5),(3.5,3.5))
True
-}
sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) | ((x3>= x1 && x3<=x2)&& ((y3>=y1 && y3<=y2) || (y4>=y1 && y4<=y2))) || ((y1==y3 &&y2==y4)&& ((x3>=x1 && x3<=x2) || (x4>=x1 && x4<=x2))) || ((x3==x1 && x4==x2)&&((y3>=y1 && y3<=y2)|| (y4>=y1 && y4<=y2))) || (x3==x2 && ((y3>=y1 && y3<=y2)|| (y4<=y2 &&y4>=y1))) || (x4==x1 && ((y4<=y2 && y4>=y1)|| (y3<=y2 && y3>=y1))) || ((x3>=x1 && x3<=x2)&&(y4<=y2 && y4>=y1)) || ((x1>=x3 && x1<=x4)&&(y2<=y4 && y2>=y3)) = True
                                                       | otherwise = False                                                                                   


{-
sobreposicao :: Hitbox -> Hitbox -> Bool
sobreposicao h1 h2 = intersecao h1 h2 || intersecao h2 h1

intersecao:: Hitbox -> Hitbox -> Bool
intersecao((p1,p2), (p3,p4)) ((p5,p6),(p7,p8)) = pointInBox (double2Float p5,double2Float p6) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p7,double2Float p8) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p5,double2Float p8) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p7,double2Float p6) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) 
-}

{-| Função que testa se uma Hitbox esta a colidir comu a lista de Hitboxs

== Exemplos

>>>colisao ((1,1),(2,2))  [((1.5,1.5),(3.5,3.5)), ((1.5,1.5),(3.5,3.5))]
True
-}
colisao :: Hitbox -> [Hitbox] -> Bool
colisao _ [] = False
colisao ((p1,p2),(p3,p4)) (((p5,p6),(p7,p8)):t) | sobreposicao ((p1,p2),(p3,p4)) ((p5,p6),(p7,p8)) = True
                                                | otherwise = colisao ((p1,p2),(p3,p4)) t

{-| Função que dada uma posição inicial e uma lista de listas de Blocos(cada lista de Blocos corresponde a uma linha ) dá a posição de todos os Blocos.

== Exemplos

>>> posicaoBlocos (0.0,0.0) [[Vazio,Escada,Plataforma],[Vazio,Escada,Plataforma]]
[(1.0,0.0),(2.0,0.0),(1.0,1.0),(2.0,1.0)]
-}
posicaoBlocos :: Posicao -> [[Bloco]] -> [Posicao]
posicaoBlocos _ [] = []
posicaoBlocos (x, y) (h:t) = posicaoBlocop (x, y) h ++ posicaoBlocos (x, y + 1) t



{-| Função que dada uma posição inicial e uma lista de Blocos(o que corresponde a uma linha de Blocos) dá a posição desses Blocos.

== Exemplos

>>> posicaoBlocop (0.0,0.0) [Vazio,Escada,Plataforma]
[(1.0,0.0),(2.0,0.0)]
-}
posicaoBlocop :: Posicao -> [Bloco] -> [Posicao]
posicaoBlocop _ [] = []
posicaoBlocop (x, y) (h:t)
    | h == Vazio = posicaoBlocop (x + 1, y) t
    | otherwise = (x, y) : posicaoBlocop (x + 1, y) t



{-| Função que dada uma posição cria a Hitbox de um Bloco

== Exemplos      

>>>espacoBloco [(0.0,0.0),(10.0,10.0),(10.0,0.0),(20.0,10.0)]

[((0.0,0.0),(10.0,10.0)),((10.0,10.0),(20.0,20.0)),((10.0,0.0),(20.0,10.0)),((20.0,10.0),(30.0,20.0))]
-}
espacoBloco :: [Posicao] -> [Hitbox]
espacoBloco [] = []
espacoBloco ((x,y):t) = ((x,y), (x + 10,y + 10)) : espacoBloco t 


{-| Função que dada uma personagem cria a sua hitbox.

== Exemplos

>>> criaHitbox per2
((3.0,4.0),(13.0,24.0))

-}
criaHitbox :: Personagem -> Hitbox
criaHitbox l = ((fst (posicao l) - fst (tamanho l)/2, snd (posicao l) - snd (tamanho l)/2 ),(fst (posicao l) + fst(tamanho l)/2, snd (posicao l) + snd (tamanho l)/2)) 

















{-| Função que dada uma lista de blocos retorna a lista das posições desses blocos -}
posicaoBlocoss :: [Bloco] -> [Posicao]
posicaoBlocoss [] = []
posicaoBlocoss (bloco:resto)
    | bloco == Plataforma || bloco == Alcapao = (0, 0) : map (\(linha, coluna) -> (linha + 1, coluna)) (posicaoBlocoss resto)
    | otherwise = posicaoBlocoss resto


{-| Função que dado um bloco e uma matriz de blocos retorna uma lista com as posições onde se encontram esses blocos -}
posicaoBlocos1 :: Bloco -> [[Bloco]] -> [Posicao] 
posicaoBlocos1 bloco matriz = [(fromIntegral linhas, fromIntegral colunas) | 
                                   colunas <- [0 .. (length matriz)-1]  
                                 , linhas  <- elemIndices bloco (matriz !! colunas)]  

{-| Função que dado um mapa e uma posição retorna o bloco que se encontra nessa posição -}
blocoNaPosicao :: Mapa -> Posicao -> Maybe Bloco
blocoNaPosicao (Mapa _ _ blocos) (x, y) | round y >= 0 && round y < length blocos && round x < length (head blocos) && x >= 0 = Just (blocos !! round y !! round x)
                                        | otherwise = Nothing
                      
                                                 
                    



