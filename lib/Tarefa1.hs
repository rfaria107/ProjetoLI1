{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

colisoesJogadorParede :: Mapa -> Personagem -> Bool
colisoesJogadorParede m p
                    |fst (posicao p) >= larguramapa || fst (posicao p) <= iniciomapa || snd (posicao p) >= alturamapa = True
                    |otherwise = False
                        where 
                                larguramapa = 600
                                iniciomapa = 0
                                alturamapa = 500

                                
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox ((x1,y1),(w1,z1)) ((x2,y2),(w2,z2))
                            |x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + z2 && y1+z1 > y2 = True
                            |otherwise = False

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisaoHitbox h1 h2
                                where h1 = defineHitbox p1
                                      h2 = defineHitbox p2

defineHitbox :: Personagem -> Hitbox 
defineHitbox p = (ci,cs) -- Hitbox é definida pelo canto inferior esquerdo e canto superior direito
                    where ci = (fst (posicao p) - snd (tamanho p),snd (posicao p) - snd (tamanho p))
                          cs = (fst (posicao p) + snd (tamanho p)/2,snd (posicao p) + (snd (tamanho p)/2))