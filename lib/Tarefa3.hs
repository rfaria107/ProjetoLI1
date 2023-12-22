{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta = undefined 

hitboxDano :: Personagem -> Hitbox -> Hitbox -- a Hitbox do dano de um jogador é uma hitbox do seu tamanho colocada a seu lado para onde este estiver virado com o martelo.
hitboxDano p@(Personagem _ _ _ dir _ _ _ _ _ _) ((x1,y1),(x2,y2)) | dir == Oeste = ((x1-l,y1),(x2-l,y2))
                                                                  | dir == Este  = ((x1+l,y1),(x2+l,y2))    
                                                                        where l = fst (tamanho p)
aplicarDanoMartelo :: Personagem -> Hitbox -> Personagem -> Hitbox -> Personagem -- a p1 é o jogador com a propriedade "aplicaDano" a True, a p2 é um Fantasma. Se a hitbox do dano colidir com a hitbox do fantasma, este perde 1 vida.
aplicarDanoMartelo p1@(Personagem _ Jogador _ _ _ _ _ _ _ (True,_)) h1 p2@(Personagem velocidade Fantasma pos dir (c,l) esc ress vidas pon (n,z)) h2    |colisaoHitbox (hitboxDano p1 h1) h2 = (Personagem velocidade Fantasma pos dir (c,l) esc ress (vidas-1) pon (n,z)) 
                                                                                                                                                        |otherwise = p2
aplicarGravidade :: Personagem -> Bloco -> Personagem
aplicarGravidade p1@(Personagem vel Jogador pos dir (c,l) esc ress vidas pon (n,z)) bloco1  |not (colisaoBloco p1 bloco1) = (Personagem gravidade Jogador pos dir (c,l) esc ress vidas pon (n,z))
                                                                                            |otherwise = p1
                                    
aplicarDanoJogador :: Personagem -> Personagem -> Personagem -- p1 é o jogador e p2 o fantasma. Se a hitbox de ambos colidir o jogador perde uma vida.
aplicarDanoJogador p1@(Personagem vel Jogador pos dir (c,l) esc ress vidas pon (n,z)) p2    | colisaoHitbox (defineHitbox p1) (defineHitbox p2) = (Personagem vel Jogador pos dir (c,l) esc ress (vidas-1) pon (n,z))
                                                                                            |otherwise = p1

