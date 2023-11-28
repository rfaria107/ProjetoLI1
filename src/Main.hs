module Main where

import LI12324 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import Mapa
import Data.Maybe (fromJust)


type Images = [Picture]
type Imagens = [(String,Picture)]
type EstadoMapa = (Jogo, Imagens)

altura :: Float
altura =  525

comprimento :: Float 
comprimento = (-940) 

l :: Int 
l = 10

getMapa :: Jogo -> Mapa
getMapa (Jogo m j d w) = m

estadoInicial :: Jogo
estadoInicial = Jogo mapa1 [] [] (Personagem (0,0) Jogador (0,0) Oeste (1,1) False False 3 0 (False,0))
reageEvento :: Event -> Jogo -> Jogo
reageEvento _ j = j

reageTempo :: Float -> Jogo -> Jogo
reageTempo _ j = j


transformarMatriz :: [[Bloco]] -> Int -> [[(Bloco,(Int,Int))]]
transformarMatriz [] x = []
transformarMatriz mapa@((h:t)) y = transformaLinha (h) (-70) y : transformarMatriz t (y+10)


transformaLinha :: [Bloco] -> Int -> Int -> [(Bloco,(Int,Int))]
transformaLinha [] _ _ = []
transformaLinha (bloco:rblocos) x y = (bloco, (x,y)) : transformaLinha rblocos (x+l) y


desenhaMapa :: EstadoMapa -> [[(Bloco,(Int,Int))]] -> Picture
desenhaMapa _ [] = blank
desenhaMapa estadogloss@((Jogo mapa _ _ _), images) (h:t) = pictures [desenhaLinhaMapa images h, desenhaMapa estadogloss t]


desenhaLinhaMapa :: Imagens -> [(Bloco, (Int,Int))] -> Picture
desenhaLinhaMapa _ [] = blank
desenhaLinhaMapa images ((h,(x,y)):t) = case h of
            p -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.5 0.5 $ fromJust $ lookup "Alcapao" images, desenhaLinhaMapa images t ]
            e -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.5 0.5 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
            v -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.5 0.5 $ fromJust $ lookup "Alcapao" images, desenhaLinhaMapa images t ]
            a -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.5 0.5 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]

desenhaEstado :: Imagens -> Jogo -> Picture
desenhaEstado images jogo@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador) = pictures [Color black $ rectangleSolid 1200 900,
                                                                                                                desenhaMapa (jogo,images) (transformarMatriz blocos (-20))]


fr :: Int 
fr = 60

carregarImagens :: IO Imagens
carregarImagens = do
               alcapao <- loadBMP "../2023li1g086/src/block.bmp"
               escadas <- loadBMP "../2023li1g086/src/escada.bmp"

               let imagens = [("Alcapao", scale 0.2 0.2 $ alcapao),
                                ("Escada", scale 0.2 0.2 $ escadas)
                              ]
               return imagens

main = do 
        images <- carregarImagens 
        play
            FullScreen                         
            yellow                 
            50                        
            (estadoInicial)
            (desenhaEstado images)        
            reageEvento               
            reageTempo                 