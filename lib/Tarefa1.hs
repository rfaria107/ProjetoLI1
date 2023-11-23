{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m p
                    |fst (posicao p) >= larguramapa || fst (posicao p) <= iniciomapa || snd (posicao p) >= alturamapa = True
                        where 
                                larguramapa = 600
                                iniciomapa = 0
                                alturamapa = 500

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens jogador p2
                            | fst (posicao jogador) + (comprimentohitbox/2) == fst (posicao p2) - (comprimentohitboxF/2) || snd (posicao jogador) == snd (posicao p2)= True
                                
                                where comprimentohitbox = fst (snd (hitbox jogador)) - fst (fst (hitbox jogador))
                                      comprimentohitboxF = fst (snd (hitbox p2)) - fst (fst (hitbox p2))