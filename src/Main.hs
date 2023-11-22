module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 

import LI12324 
import Data.List 



type Estado = Coordenadas
type Coordenadas = (Float, Float)




estadoInicial :: Estado
estadoInicial = (0,0)



estadoGlossInicial :: [Personagem]-> Mapa -> Jogo
estadoGlossInicial ls mapa = Jogo mapa    









get_images :: IO [Picture]
get_images = do
               pac_open <- loadBMP "pac_open.bmp"
               pac_closed <- loadBMP "pac_closed.bmp"
               let images = [scale 1.5 1.5 pac_open, scale 1.5 1.5 pac_closed]
               return images


dm :: Display
dm = InWindow
       "Novo Jogo"  -- título da janela
       (400, 400)   -- dimensão da janela
       (0,0)        -- posição no ecran

fr :: Int
fr = 50

main :: IO ()
main = do 
        imagens <- get_images
        play  dm                          -- janela onde irá decorrer o jogo
              (greyN 0.5)                 -- cor do fundo da janela
              fr                          -- frame rate
              (estadoGlossInicial imagens)  -- define estado inicial do jogo
              desenhaEstado               -- desenha o estado do jogo
              reageEvento                 -- reage a um evento
              reageTempo      