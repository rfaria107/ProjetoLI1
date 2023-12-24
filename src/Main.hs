module Main where

import LI12324 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import Mapa
import Data.Maybe (fromJust)
import Tarefa3
import Tarefa1


type Images = [Picture]
type Imagens = [(String,Picture)]
type EstadoMapa = (Jogo, Imagens)

hitboxLargura :: Float
hitboxLargura = 70

hitboxAltura :: Float
hitboxAltura = 89

altura :: Float
altura =  525

comprimento :: Float 
comprimento = (-940) 

l :: Int 
l = 10

getMapa :: Jogo -> Mapa
getMapa (Jogo m j d w) = m


estadoInicial :: Imagens -> Jogo
estadoInicial imagens =
    Jogo mapa1 [] [] (Personagem (0,0) Jogador (-40,-33.1) Oeste (1,1) False False 3 0 (False,0))


reageEvento :: Event -> Jogo -> Jogo
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) =  (j { jogador = Personagem pos t (x - 1, y) Oeste tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) =   (j { jogador = Personagem pos t (x + 1, y) Este tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = (j { jogador = Personagem pos t (x , y + 1) Este tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = (j { jogador = Personagem pos t (x , y - 1) Este tam e r v p (c, d) }) 
reageEvento _ j = j 


aplicaGravidade :: Float -> Jogo -> Jogo
aplicaGravidade _ jogo@(Jogo mapa inimigos colecionaveis jogador) =
    jogo { jogador = jogador { posicao = (xs, ys+vy+(-0.06))} }
  where
    (xs, ys) = posicao jogador  
    (vx, vy) = velocidade jogador 


reageTempo :: Float -> Jogo -> Jogo
reageTempo dt j@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador) = if  colisoesJogadorParede mapa jogador == True then j else  aplicaGravidade dt j


desenhaJogador :: Imagens -> Personagem -> Picture
desenhaJogador images j@(Personagem _ Jogador (x, y) direcao tamanho _ _ _ _ _) =
    Pictures [ Translate (realToFrac x * realToFrac l) (realToFrac y * realToFrac l) $ Scale 1 1 $ fromJust $ lookup "Jogador" images , defineHitboxJ j]

defineHitboxJ :: Personagem -> Picture
defineHitboxJ (Personagem _ Jogador (x, y) direcao tamanho _ _ _ _ _) = Color green $ Translate (realToFrac x * realToFrac l) (realToFrac y * realToFrac l) $ rectangleWire hitboxLargura hitboxAltura

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
                Plataforma -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.3 0.3 $ fromJust $ lookup "Plataforma" images, desenhaLinhaMapa images t ]
                Escada -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale  2.5 2.5 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
                Vazio -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.3 0.3 $ fromJust $ lookup "Vazio" images, desenhaLinhaMapa images t ]
                Alcapao -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 1 1 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]


desenhaEstado :: Imagens -> Jogo -> Picture
desenhaEstado images jogo@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador) =
    pictures [Color black $ rectangleSolid 1200 900,
              desenhaMapa (jogo, images) (fazMatriz blocos (-20)),
              desenhaJogador images jogador]


fr :: Int 
fr = 60

carregarImagens :: IO Imagens
carregarImagens = do
        plataforma <- loadBMP "../2023li1g086/resources/block.bmp"
        escadas <- loadBMP "../2023li1g086/resources/Ladder.bmp"
        alcapao <- loadBMP "../2023li1g086/resources/alcapao.bmp"
        jogador <- loadBMP "../2023li1g086/resources/player1.bmp"
        let imagens = [ ("Plataforma", scale 0.43 0.2 $ plataforma),
                        ("Escada", scale 0.43 0.5 $ escadas),
                        ("Alcapao", scale 0.52 0.36 $ alcapao),
                        ("Vazio", Blank),
                        ("Jogador", scale 0.33 0.33 $ jogador)
                        ]
        
        return imagens

main = do 
        images <- carregarImagens 
        play 
                FullScreen                         
                yellow               
                50                        
                (estadoInicial images)
                (desenhaEstado images)        
                reageEvento               
                reageTempo