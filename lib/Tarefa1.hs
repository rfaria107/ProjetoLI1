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
colisoesJogadorParede (Mapa _ _ (l1:ls)) p
                  |fst (snd (defineHitbox p)) >= fromIntegral (length l1) || fst (fst (defineHitbox p)) <= 0 || snd (snd (defineHitbox p)) >= 0 = True  --laterais e topo do mapa 
                  |otherwise = False
 
defineHitbox :: Personagem -> Hitbox  -- Função que, dada uma personagem, define a sua hitbox
defineHitbox p = (ci,cs) -- Hitbox é definida pelo canto inferior esquerdo e canto superior direito
                    where ci = (fst (posicao p) - fst (tamanho p)/2 , snd (posicao p) - snd (tamanho p)/2)
                          cs = (fst (posicao p) + fst (tamanho p)/2 , snd (posicao p) + (snd (tamanho p)/2))

colisaoHitbox :: Hitbox -> Hitbox -> Bool -- Função que verifica se duas hitboxes se sobrepõem
colisaoHitbox ((x1,y1),(w1,z1)) ((x2,y2),(w2,z2))
                            |x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + z2 && y1+z1 > y2 = True
                            |otherwise = False

colisoesPersonagens :: Personagem -> Personagem -> Bool -- Função que recebe duas personagens e aplica a função que verifia se as suas hitboxes se sobrepõem
colisoesPersonagens p1 p2 = colisaoHitbox h1 h2
                                where h1 = defineHitbox p1
                                      h2 = defineHitbox p2

hitboxBloco :: Posicao -> Bloco -> Hitbox
hitboxBloco (x,y) bloco = if bloco == Plataforma then ((x-0.5,y-0.5),(x+0.5,y+0.5)) else ((x,y),(x,y))

colisaoBloco :: Personagem -> Bloco -> Bool
colisaoBloco p1@(Personagem _ _ (x,y) _ _ _ _ _ _ _) b1 | b1 == Plataforma = colisaoHitbox (defineHitbox p1) (hitboxBloco (x,y-0.5) b1)

{-
colisoesPlataforma :: Personagem -> Mapa -> Bool -- Função que deteta colisões com plataformas
colisoesPlataforma p1@(Personagem _ _ posicao1 _ _ _ _ _ _ _) (Mapa _ _ (matriz1@(b1:bs):ls) = any colisaoHitbox p1 blocosMatriz
                                                                                                where blocosMatriz = concat matriz1
                                                                                                       if bloco == Plataforma = colisaoHitbox (defineHitbox p1) (hitboxBloco posicao1 b1)
-}