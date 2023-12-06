{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1
valida :: Jogo -> Bool
valida jogo1 = validaMapa && validaInimigos && validaPos && validaNumP
                    
validaMapa :: Mapa -> Bool -- verifica se o mapa tem chão
validaMapa (Mapa ((xi, yi), dir) (xf, yf) matriz) = all (== Plataforma) (last matriz) -- Verficar se a última linha do mapa é constituida por elementos do tipo "Plataforma" e se a posição inicial é diferente da final

validaRessalto :: [Personagem] -> Bool -- verifica se os inimigos têm a propriedade ressalta True e o jogador False
validaRessalto lp@(p:ps) = all aux lp
                        where aux p = if tipo p == Jogador then not (ressalta p) else ressalta p

validaPos :: Personagem -> Personagem -> Bool
validaPos p1 p2 = not (colisaoHitbox h1 h2)
                where h1 = defineHitbox p1
                      h2 = defineHitbox p2

validaNumP :: [Personagem] -> Bool
validaNumP p = length p > 2                            

validaVidaFantasma :: Personagem -> Bool
validaVidaFantasma p1@(Personagem _ Fantasma (x,y) _ _ _ _ vidas _ _) = vidas == 1