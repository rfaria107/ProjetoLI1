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
import Data.List
valida :: Jogo -> Bool
valida jogo1@(Jogo mapa1 inimigos@(i1:is) colecionaveis jogador ) = validaMapa mapa1 && validaRessalto (inimigos++[jogador]) && validaPos i1 jogador && validaNumP inimigos && validaVidaFantasma jogo1 && validaColecionaveis jogo1
                    
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
validaNumP p = length p >= 2                            

validaVidaFantasma :: Jogo -> Bool
validaVidaFantasma (Jogo _(p1@(Personagem _ Fantasma (x,y) _ _ _ _ vidas _ _):xs) _ _ ) = vidas == 1 

--validaEscadas :: Mapa -> Bool
--validaEscadas (x,y) (Mapa _ _ matriz) = 

validaColecionaveis :: Jogo -> Bool 
validaColecionaveis  (Jogo (Mapa _ _ matriz) _ listac _ ) = 
     all (\(colecionavel1, (xc,yc)) ->  blocoNaPosicao (xc,yc) matriz == Vazio) listac
blocoNaPosicao :: Posicao -> [[Bloco]] -> Bloco -- função que, após receber uma posição e uma matriz retorna qual o tipo de bloco nesta posição da matriz
blocoNaPosicao (x,y) matriz = (matriz !! floor y) !! floor x

--encontraPlataforma :: Mapa -> [Posicao]
--encontraPlataforma (Mapa _ _ [[blocos]])= (10 ,(elemIndices Plataforma [blocos]))