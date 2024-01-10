{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import LI12324
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesinimigos eventosAcao jogo1 = jogo1 {
    jogador = moveJogador (velocidadeJogador (jogador jogo1) eventosAcao)
                                    }

eventosAcao :: Event -> Maybe Acao
eventosAcao (EventKey (SpecialKey KeySpace) Down _ _) = Just Saltar
eventosAcao (EventKey (SpecialKey KeyUp)Down _ _) = Just Subir
eventosAcao (EventKey (SpecialKey KeyRight) Down _ _) = Just AndarDireita
eventosAcao (EventKey (SpecialKey KeyLeft)Down _ _) = Just AndarEsquerda
eventosAcao (EventKey (SpecialKey KeyDown) Down _ _) = Just Descer
eventosAcao _ = Nothing

velocidadeJogador :: Personagem -> Maybe Acao -> Personagem
velocidadeJogador j1@(Personagem (x,y) Jogador pos dir (c,l) esc ress vidas pon (n,z)) acao
                    | acao == Just Saltar = j1 {
                        velocidade = (x,y-1)
                    }
                    | acao == Just AndarDireita = j1 {
                        velocidade = (x+1,y)
                    }
                    | acao == Just AndarEsquerda = j1 {
                        velocidade = (x-1,y)
                    }

moveJogador :: Personagem -> Personagem
moveJogador j1@(Personagem (vx,vy) Jogador (x,y) dir (c,l) esc ress vidas pon (n,z))
                                                                                |vx>1 = Personagem (vx-1,vy) Jogador (x+1,y) dir (c,l) esc ress vidas pon (n,z)
                                                                                |vx<(-1) = Personagem (vx+1,vy) Jogador (x-1,y) dir (c,l) esc ress vidas pon (n,z)
                                                                                |vy>1 = Personagem (vx,vy) Jogador (x,y-1) dir (c,l) esc ress vidas pon (n,z)