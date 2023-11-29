module Main where

import LI12324 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import Mapa
import Data.Maybe


type Images = [Picture]
type Imagens = [(String,Picture)]
type EstadoMapa = (Jogo, Imagens)

desenhaPlayer :: Picture
desenhaPlayer = Color blue $ rectangleSolid 100 100


desenhaFantasma :: Picture 
desenhaFantasma = Color white $ rectangleSolid 100 100

altura :: Float
altura =  720

comprimento :: Float 
comprimento = (-1280)      

l :: Int 
l = 10

getMapa :: Jogo -> Mapa
getMapa (Jogo m j d w) = m

estadoInicial :: Jogo
estadoInicial = Jogo mapa1 [] [] (Personagem (0,0) Jogador (100,620) Este (100,100) False False 3 0 (False,0))

reageEvento :: Event -> Jogo -> Jogo
reageEvento _ j = j

reageTempo :: Float -> Jogo -> Jogo
reageTempo _ j = j

fazMatriz :: [[Bloco]] -> Int -> [[(Bloco,(Int,Int))]]
fazMatriz [] x = []
fazMatriz mapa@((h:t)) y = fazLinha h (-128) y : fazMatriz t (y+128)

fazLinha :: [Bloco] -> Int -> Int -> [(Bloco,(Int,Int))]
fazLinha [] _ _ = []
fazLinha (bloco:rblocos) x y = (bloco, (x,y)) : fazLinha rblocos (x+l) y


desenhaMapa :: EstadoMapa -> [[(Bloco,(Int,Int))]] -> Picture
desenhaMapa _ [] = blank
desenhaMapa estadogloss@(Jogo mapa _ _ _, images) (h:t) = pictures [desenhaLinhaMapa images h, desenhaMapa estadogloss t]


desenhaLinhaMapa :: Imagens -> [(Bloco, (Int,Int))] -> Picture
desenhaLinhaMapa _ [] = blank
desenhaLinhaMapa images ((h,(x,y)):t) = case h of
            p -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.1 0.1 $ fromJust $ lookup "Plataforma" images, desenhaLinhaMapa images t ]
            e -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 1 1 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
            v -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 1 1 $ fromJust $ lookup "Vazio" images, desenhaLinhaMapa images t ]
            a -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 1 1 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]

desenhaEstado :: Imagens -> Jogo -> Picture
desenhaEstado images jogo@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador) = pictures [desenhaMapa (jogo,images) (fazMatriz blocos (-128)), translate 100 100 desenhaPlayer, desenhaFantasma]


fr :: Int 
fr = 60

carregarImagens :: IO Imagens
carregarImagens = do
        plataforma <- loadBMP "../2023li1g086/resources/block.bmp"
        escadas <- loadBMP "../2023li1g086/resources/Ladder.bmp"
        alcapao <- loadBMP "../2023li1g086/resources/alcapao.bmp"
        let imagens = [  ("Plataforma", scale 1 1 $ plataforma),
                          ("Escada", scale 1 1 $ escadas),
                          ("Alcapao", scale 1 1 $ alcapao),
                          ("Vazio", Blank)
                        ]
          
        return imagens

main = do 
        images <- carregarImagens
        play
            FullScreen                         
            black            
            50                        
            estadoInicial
            (desenhaEstado images)        
            reageEvento               
            reageTempo                 