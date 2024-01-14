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
    jogador = (velocidadeJogador (jogador jogo1) acao),
    inimigos = velocidadeinimigos (inimigos jogo1) (geraAcoes 2 3)
                                    }
                                    
velocidadeJogador :: Personagem -> Maybe Acao -> Personagem
velocidadeJogador j1@(Personagem (vx,vy) Jogador (x,y) dir (c,l) esc ress vidas pon (n,z)) acao
                    | acao == Just Saltar = j1 {
                        posicao = (x,y-0.5)
                    }
                    | acao == Just AndarDireita = j1 {
                        velocidade = (0.1,vy)
                    }
                    | acao == Just AndarEsquerda = j1 {
                        velocidade = (-0.1,vy)
                    }
                    | acao == Just Subir = j1 {
                        velocidade = (vx,-0.1)
                    }
                    | acao == Just Descer = j1 {
                        velocidade = (vx,0.1)
                    }
                    | acao == Just Parar = j1 {
                        velocidade = (0,0)
                    }
                    | acao == Nothing = j1 {
                        velocidade = (vx,vy)
                    }
                    
velocidadeinimigos :: [Personagem] -> [Maybe Acao] -> [Personagem]
velocidadeinimigos [] _ = []
velocidadeinimigos (inimigo1@(Personagem (vx,vy) MacacoMalvado _ _ _ _ _ _ _ _):is) (acao:as) = inimigo1 {
                                        velocidade = (0,0)} : velocidadeinimigos is as
velocidadeinimigos (inimigo1@(Personagem (vx,vy) Fantasma _ _ _ _ _ _ _ _):is) (acao:as)
                                    | acao == Just AndarDireita = inimigo1 {
                                        velocidade = (0.1,vy)
                                    } : velocidadeinimigos is as
                                    | acao == Just AndarEsquerda = inimigo1 {
                                        velocidade = (-0.1,vy)
                                    } : velocidadeinimigos is as
                                    | acao == Nothing = inimigo1 {
                                        velocidade = (vx,vy)
                                    } : velocidadeinimigos is as

geraAcao :: Int -> Maybe Acao
geraAcao randomInt 
                    |odd randomInt = Just AndarDireita
                    |even randomInt = Just AndarEsquerda
                    |otherwise = Nothing

geraAcoes :: Semente -> Int -> [Maybe Acao]
geraAcoes s c = map geraAcao  (geraAleatorios s c)
