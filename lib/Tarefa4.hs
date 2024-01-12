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
atualiza acoesinimigos acao jogo1 = jogo1 {
    jogador = (velocidadeJogador (jogador jogo1) acao)
                                    }
                                    
velocidadeJogador :: Personagem -> Maybe Acao -> Personagem
velocidadeJogador j1@(Personagem (vx,vy) Jogador pos dir (c,l) esc ress vidas pon (n,z)) acao
                    | acao == Just Saltar = j1 {
                        velocidade = (vx,vy+1)
                    }
                    | acao == Just AndarDireita = j1 {
                        velocidade = (1,vy)
                    }
                    | acao == Just AndarEsquerda = j1 {
                        velocidade = (-1,vy)
                    }
                    | acao == Just Subir = j1 {
                        velocidade = (vx,vy+1)
                    }
                    | acao == Just Descer = j1 {
                        velocidade = (vx,vy-1)
                    }
                    | acao == Just Parar = j1 {
                        velocidade = (0,0)
                    }
                    | acao == Nothing = j1 {
                        velocidade = (vx,vy)
                    }