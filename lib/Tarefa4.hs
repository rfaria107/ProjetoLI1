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
                        velocidade = (vx,vy-0.5)
                    }
                    | acao == Just AndarDireita = j1 {
                        velocidade = (0.1,vy)
                    }
                    | acao == Just AndarEsquerda = j1 {
                        velocidade = (-0.1,vy)
                    }
                    | acao == Just Subir = j1 {
                        velocidade = (vx,vy-0.1)
                    }
                    | acao == Just Descer = j1 {
                        velocidade = (vx,vy+0.1)
                    }
                    | acao == Just Parar = j1 {
                        velocidade = (0,0)
                    }
                    | acao == Nothing = j1 {
                        velocidade = (vx,vy)
                    }
moverinimigos :: [Personagem] -> [Maybe Acao] -> [Personagem]
moverinimigos (inimigo1@(Personagem (vx,vy) Fantasma _ _ _ _ _ _ _ _):is) (acao:as)
                                    | acao == Just Saltar = inimigo1 {
                                        velocidade = (vx,vy-1)
                                    } : moverinimigos is as
                                    | acao == Just AndarDireita = inimigo1 {
                                        velocidade = (1,vy)
                                    } : moverinimigos is as
                                    | acao == Just AndarEsquerda = inimigo1 {
                                        velocidade = (-1,vy)
                                    } : moverinimigos is as
                                    | acao == Just Subir = inimigo1 {
                                        velocidade = (vx,vy-1)
                                    } : moverinimigos is as
                                    | acao == Just Descer = inimigo1 {
                                        velocidade = (vx,vy+1)
                                    } : moverinimigos is as
                                    | acao == Just Parar = inimigo1 {
                                        velocidade = (0,0)
                                    } : moverinimigos is as
                                    | acao == Nothing = inimigo1 {
                                        velocidade = (vx,vy)
                                    } : moverinimigos is as

