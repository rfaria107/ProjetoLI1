module Main where

import LI12324 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import Mapa
import Data.Maybe (fromJust)
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import GHC.Float (float2Double, double2Float)


type Images = [Picture]
type Imagens = [(String,Picture)]
type EstadoMapa = (Jogo, Imagens)

hitboxLarguraj :: Float
hitboxLarguraj = 70

hitboxAlturaj :: Float
hitboxAlturaj = 89

hitboxLarguraO :: Float
hitboxLarguraO =  60

hitboxAlturaO :: Float
hitboxAlturaO = 100


altura :: Float
altura =  525

comprimento :: Float 
comprimento = (-940) 

l :: Int 
l = 10

desenhaMenu :: Imagens -> Picture
desenhaMenu imagens = Translate 0 100 $ fromJust $ lookup "Menu" imagens 


inimigo1 = Personagem (0,0) Fantasma (50,-35.5) Este (1,1) False True 1 0 (False,0)


inimigo2 = Personagem (0,0) Fantasma (6,10) Oeste (1,1) False True 1 0 (False,0)


jogador1 = Personagem (0,0) Jogador (2,6) Oeste (1,1) False False 3 0 (False,0)

janela :: Display
janela = InWindow "janela" (1920,1080) (0,0)

fr :: Int
fr = 60

definejogo1 :: Jogo
definejogo1 = Jogo m i c j
                where   m = mapa1
                        i = [inimigo1,inimigo2]
                        c = [(Moeda,(3,7)),(Martelo,(6,8))]
                        j = jogador1
traduzPosicao :: Posicao -> Posicao -- traduz uma posicao da lógica da matriz para esta ser utilizada no gloss
traduzPosicao (x,y) = (x*1920/20, ( - y * 1080 /10))


getMapa :: Jogo -> Mapa
getMapa j = mapa1

estadoInicial :: Imagens -> Jogo
estadoInicial imagens = definejogo1

reageEvento :: Event -> Jogo -> Jogo
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) menu = definejogo1
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) definejogo1 = atualiza [Nothing, Nothing] (Just Saltar) definejogo1
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) definejogo1 = atualiza [Nothing, Nothing] (Just AndarEsquerda) definejogo1
reageEvento (EventKey (SpecialKey KeyLeft) Up _ _) definejogo1 = atualiza [Nothing, Nothing] (Just Parar) definejogo1
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) definejogo1 = atualiza [Nothing, Nothing] (Just AndarDireita) definejogo1
reageEvento (EventKey (SpecialKey KeyRight) Up _ _) definejogo1 = atualiza [Nothing, Nothing] (Just Parar) definejogo1
--escadas
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) definejogo1 = if colisaoPersonagemEscada (jogador definejogo1) definejogo1 then atualiza [Nothing,Nothing] (Just Subir) definejogo1 else definejogo1
reageEvento _ j = j 


reageTempo :: Float -> Jogo -> Jogo
reageTempo dt definejogo1 = movimenta 1 (float2Double dt) $ definejogo1

desenhaJogador :: Imagens -> Personagem -> Picture
desenhaJogador images j@(Personagem _ Jogador pos direcao tamanho _ _ _ _ _) =
    Pictures [ Translate ((double2Float (fst (traduzPosicao pos))) * double2Float (snd (traduzPosicao pos))) (double2Float (snd (traduzPosicao pos) * realToFrac l)) $ Scale 1 1 $ fromJust $ lookup "Jogador" images , defineHitboxJ j]

desenhaObjetivo :: Imagens -> [(Colecionavel, Posicao)] -> Picture
desenhaObjetivo _ [] = blank
desenhaObjetivo images (w@(Moeda, pos):t) = Pictures [Translate (double2Float (fst (traduzPosicao pos)) * realToFrac l) (realToFrac (snd (traduzPosicao pos)) * realToFrac l) $ fromJust $ lookup "Peach" images, defineHitboxO w ]
desenhaObjetivo images ((_, _):t) = desenhaObjetivo images t

desenhaInimigos :: Imagens -> [Personagem] -> Picture 
desenhaInimigos _ [] = blank
desenhaInimigos images (i@(Personagem _ Fantasma pos direcao tamanho _ _ _ _ _):t) =  Pictures [ Translate (double2Float (fst (traduzPosicao pos)) * realToFrac l) (double2Float (snd (traduzPosicao pos))* realToFrac l) $ Scale 1 1 $ fromJust $ lookup "Fantasma" images]
desenhaInimigos images (h:t) = desenhaInimigos images t 


defineHitboxJ :: Personagem -> Picture
defineHitboxJ (Personagem _ Jogador pos direcao tamanho _ _ _ _ _) = Color green $ Translate (double2Float (fst (traduzPosicao pos)) * realToFrac l) (double2Float (snd (traduzPosicao pos)) * realToFrac l) $ rectangleWire hitboxLarguraj hitboxAlturaj

defineHitboxO :: (Colecionavel,Posicao) -> Picture 
defineHitboxO (Moeda,pos) = Color green $ Translate (double2Float (fst (traduzPosicao pos)) * realToFrac l) (double2Float (snd (traduzPosicao pos)) * realToFrac l) $ rectangleWire hitboxLarguraO hitboxAlturaO

fazMatriz :: [[Bloco]] -> Int -> [[(Bloco,(Int,Int))]]
fazMatriz [] x = []
fazMatriz mapa@((h:t)) y = fazLinha (h) (-45) y : fazMatriz t (y+10)

fazLinha :: [Bloco] -> Int -> Int -> [(Bloco,(Int,Int))]
fazLinha [] _ _ = []
fazLinha (bloco:rblocos) x y = (bloco, (x,y)) : fazLinha rblocos (x+l) y

desenhaMapa :: EstadoMapa -> [[(Bloco,(Int,Int))]] -> Picture
desenhaMapa _ [] = blank
desenhaMapa e@((Jogo mapa _ _ _), images) (h:t) = pictures [desenhaLinhaMapa images h, desenhaMapa e t]

desenhaLinhaMapa :: Imagens -> [(Bloco, (Int,Int))] -> Picture
desenhaLinhaMapa _ [] = blank
desenhaLinhaMapa images ((h,(x,y)):t) = case h of 
                Plataforma -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.3 0.3 $ fromJust $ lookup "Plataforma" images, desenhaLinhaMapa images t ]
                Escada -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale  2.5 2.5 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
                Vazio -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 0.3 0.3 $ fromJust $ lookup "Vazio" images, desenhaLinhaMapa images t ]
                Alcapao -> Pictures [Translate ((fromIntegral x)*(realToFrac l)) (-(fromIntegral y)*(realToFrac l)) $ Scale 1 1 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]


desenhaEstado :: Imagens -> Jogo -> Picture 
desenhaEstado images jogo@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador)   
                        = pictures [Color black $ rectangleSolid 1200 900,
                        desenhaMapa (jogo, images) (fazMatriz blocos (-20)),
                        desenhaJogador images jogador,
                        desenhaObjetivo images colecionaveis,
                        desenhaInimigos images inimigos]


carregarImagens :: IO Imagens
carregarImagens = do
        plataforma <- loadBMP "../2023li1g086/resources/block.bmp"
        escadas <- loadBMP "../2023li1g086/resources/Ladder.bmp"
        alcapao <- loadBMP "../2023li1g086/resources/alcapao.bmp"
        jogador <- loadBMP "../2023li1g086/resources/player1.bmp"
        objetivo <- loadBMP "../2023li1g086/resources/goal.bmp"
        fantasma <- loadBMP "../2023li1g086/resources/Fantasma.bmp"
        menuinicial <- loadBMP "../2023li1g086/resources/menukong.bmp"
        let imagens = [ ("Plataforma", scale 0.43 0.2 $ plataforma),
                        ("Escada", scale 0.43 0.5 $ escadas),
                        ("Alcapao", scale 0.52 0.36 $ alcapao),
                        ("Vazio", Blank),
                        ("Jogador", scale 0.33 0.33 $ jogador),
                        ("Peach", scale 0.2 0.2 $ objetivo),
                        ("Fantasma", scale 0.2 0.2 $ fantasma),
                        ("Menu", scale 0.2 0.2 $ menuinicial)
                        ]
        
        return imagens

main = do 
        images <- carregarImagens 
        play 
                janela                         
                black               
                fr                      
                (estadoInicial images)
                (desenhaEstado images)        
                reageEvento               
                reageTempo
                
{-
colisaoPlataforma :: Jogo -> Bool
colisaoPlataforma j@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador)    | y<=(-33.1) && y>=(-33.9) && x>=(-51.7) && x<=(51.9) = True 
                                                                                                | (y>=(-4) && y<=(-3) && x>=(-51.7) && x<=(-7.7)) || (x>= (-2.4) && x<=(51.9) && y>=(-3.5) && y<=(-3)) = True
                                                                                                | otherwise = False
                where 
                    (x,y) = posicao jogador


colisaoEscada :: Jogo -> Bool
colisaoEscada j@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador) | y>=(-100) && y<=(-3.1) && x>=(33) && x<=(35) = True
                                                                                         | otherwise = False  
                                                                where 
                                                                    (x,y) = posicao jogador 

moveFantasmas :: [Personagem] -> [Personagem]
moveFantasmas [] = []
moveFantasmas ((Personagem v Fantasma (x, y) d tam e r w p (False, 0)):t) = (Personagem v Fantasma (newX, y) newD tam e r w p (False, 0)) : moveFantasmas t
                                        where 
                                            newX = if x >= 51.9 then -51.7 else if x <= -51.7 then 51.9 else x + (if d == Este then 0.5 else -0.5)
                                            newD = if newX >= 51.9 || newX <= -51.7 then if d == Este then Oeste else Este else d
moveFantasmas (h:t) = h : moveFantasmas t

reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoEscada j then j else (j { jogador = Personagem pos t (x - 3.5, y) Oeste tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoEscada j then j else  (j { jogador = Personagem pos t (x + 3.5, y) Este tam e r v p (c, d) })
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoPlataforma j || colisaoEscada j then (j { jogador = Personagem pos t (x , y + 10) Este tam e r v p (c, d) }) else j
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoPlataforma j || (colisaoEscada j == False)  then j else (j { jogador = Personagem pos t (x , y - 5) Este tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoPlataforma j || (colisaoEscada j == False)  then j else (j { jogador = Personagem pos t (x , y - 5) Este tam e r v p (c, d) })


aplicaGravidade :: Float -> Jogo -> Jogo
aplicaGravidade _ jogo@(Jogo mapa inimigos colecionaveis jogador) = if colisaoPlataforma jogo then jogo else jogo { jogador = jogador { posicao = (xs, ys+vy+(-0.4))} }
    where
        (xs, ys) = posicao jogador  
        (vx, vy) = velocidade jogador 
-}