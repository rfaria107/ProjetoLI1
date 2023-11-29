{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

--valida :: Jogo -> Bool
--valida jogo1
--                |validaMapa && validaInimigos && validaPos = True
--                |otherwise = False

validaMapa :: Mapa -> Bool
validaMapa (Mapa ((xi, yi), dir) (xf, yf) matriz)
                                                        |all (== Plataforma) (last matriz) = True -- Verficar se a última linha do mapa é constituida por elementos do tipo "Plataforma" e se a posição inicial é diferente da final

validaInimigos :: [Personagem] -> Bool
validaInimigos [] = False

--validaPos :: Personagem -> [Personagem] -> Bool


