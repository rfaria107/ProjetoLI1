{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

colisoesJogadorParede :: Mapa -> Personagem -> Bool -- Função que verifica se um personagem se encontra dentro dos limites do mapa
colisoesJogadorParede m p
                    |fst (snd (defineHitbox p)) >= larguramapa || fst (fst (defineHitbox p)) <= iniciomapa || snd (snd (defineHitbox p)) >= alturamapa = True --laterais e topo do mapa 
                    |otherwise = False
                        where 
                                larguramapa = 1920
                                iniciomapa = 0
                                alturamapa = 1080

colisoesPlataforma :: Personagem -> Bloco -> Hitbox -> Hitbox -> Bool -- Função que deteta colisões com plataformas
colisoesPlataforma p1 b h1 hbloco
                              | colisaoHitbox h1 hbloco = True
                              | otherwise = False

                                    where 
                                          h1 = defineHitbox p1
  --                                        hbloco = defineHitboxBloco b

--defineHitboxBloco :: Bloco -> Hitbox
--defineHitboxBloco b = (ci,cs) -- Hitbox é definida pelo canto inferior esquerdo e canto superior direito do bloco
--                    where ci = (fst (posicao b) - snd (tamanho b),snd (posicao b) - snd (tamanho b))
--                          cs = (fst (posicao b) + snd (tamanho b)/2,snd (posicao b) + (snd (tamanho b)/2))


defineHitbox :: Personagem -> Hitbox  -- Função que, dada uma personagem, define a sua hitbox
defineHitbox p = (ci,cs) -- Hitbox é definida pelo canto inferior esquerdo e canto superior direito
                    where ci = (fst (posicao p) - snd (tamanho p) , snd (posicao p) - snd (tamanho p))
                          cs = (fst (posicao p) + snd (tamanho p)/2 , snd (posicao p) + (snd (tamanho p)/2))

colisaoHitbox :: Hitbox -> Hitbox -> Bool -- Função que verifica se duas hitboxes se sobrepõem
colisaoHitbox ((x1,y1),(w1,z1)) ((x2,y2),(w2,z2))
                            |x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + z2 && y1+z1 > y2 = True
                            |otherwise = False

colisoesPersonagens :: Personagem -> Personagem -> Bool -- Função que recebe duas personagens e aplica a função que verifia se as suas hitboxes se sobrepõem
colisoesPersonagens p1 p2 = colisaoHitbox h1 h2
                                where h1 = defineHitbox p1
                                      h2 = defineHitbox p2