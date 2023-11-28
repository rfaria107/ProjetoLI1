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


fazMatriz :: [[Bloco]] -> Int -> [[(Bloco,(Int,Int))]]
fazMatriz [] x = []
fazMatriz mapa@((h:t)) y = fazLinha (h) (-45) y : fazMatriz t (y+10)


fazLinha :: [Bloco] -> Int -> Int -> [(Bloco,(Int,Int))]
fazLinha [] _ _ = []
fazLinha (bloco:rblocos) x y = (bloco, (x,y)) : fazLinha rblocos (x+l) y


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
                                                                                                                desenhaMapa (jogo,images) (fazMatriz blocos (-20))]


fr :: Int 
fr = 60

carregarImagens :: IO Imagens
carregarImagens = do
        plataforma <- loadBMP "../2023li1g086/src/block.bmp"
        escadas <- loadBMP "../2023li1g086/src/Ladder.bmp"
        alcapao <- loadBMP "../2023li1g086/src/alcapao.bmp"
        let imagens = [  ("Plataforma", scale 0.43 0.2 $ plataforma),
                          ("Escada", scale 0.43 0.5 $ escadas),
                          ("Alcapao", scale 0.43 0.43 $ alcapao),
                          ("Vazio", Blank)
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