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
import Graphics.Gloss (Picture (Blank))

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed tempo jogo1 =  jogo1 {
      jogador = pontosApanharMoeda (jogador jogo1) jogo1,
      inimigos = map (aplicarDanoMartelo (jogador jogo1)) (inimigos jogo1)
}

hitboxDano :: Personagem -> Hitbox -- a Hitbox do dano de um jogador é uma hitbox do seu tamanho colocada a seu lado para onde este estiver virado com o martelo.
hitboxDano p@(Personagem _ _ _ dir _ _ _ _ _ _) | dir == Oeste = ((x1-l,y1),(x2-l,y2))
                                                | dir == Este  = ((x1+l,y1),(x2+l,y2))    
                                                                        where l = fst (tamanho p)
                                                                              ((x1,y1),(x2,y2)) = defineHitbox p
aplicarDanoMartelo :: Personagem -> Personagem -> Personagem -- a p1 é o jogador com a propriedade "aplicaDano" a True, a p2 é um Fantasma. Se a hitbox do dano colidir com a hitbox do fantasma, este perde 1 vida.
aplicarDanoMartelo p1@(Personagem _ Jogador _ _ _ _ _ _ _ (True,_)) p2@(Personagem velocidade Fantasma pos dir (c,l) esc ress vidas pon (n,z))        |vidas >= 1 && colisaoHitbox (hitboxDano p1) (defineHitbox p2) = (Personagem velocidade Fantasma pos dir (c,l) esc ress (vidas-1) pon (n,z)) 
                                                                                                                                                      |otherwise = p2
aplicarGravidade :: Personagem -> Bloco -> Personagem
aplicarGravidade p1@(Personagem vel Jogador pos dir (c,l) esc ress vidas pon (n,z)) bloco1  |not (colisaoBloco p1 bloco1) = (Personagem gravidade Jogador pos dir (c,l) esc ress vidas pon (n,z))
                                                                                            |otherwise = p1
                                    
aplicarDanoJogador :: Personagem -> Personagem -> Personagem -- p1 é o jogador e p2 o fantasma. Se a hitbox de ambos colidir o jogador perde uma vida.
aplicarDanoJogador p1@(Personagem vel Jogador pos dir (c,l) esc ress vidas pon (n,z)) p2@(Personagem velocidade2 Fantasma pos2 dir2 (c2,l2) esc2 ress2 vidas2 pon2 (n2,z2))   |vidas2 >0 && colisaoHitbox (defineHitbox p1) (defineHitbox p2) = (Personagem vel Jogador pos dir (c,l) esc ress (vidas-1) pon (n,z))
                                                                                                                                                                              |otherwise = p1

pontosApanharMoeda :: Personagem -> Jogo -> Personagem
pontosApanharMoeda p1@(Personagem vel Jogador (x,y) dir (c,l) esc ress vidas pon (n,z)) j1@(Jogo _ _ col _)     |any (==True) (map (colisaoHitbox (defineHitbox p1)) (hitboxesColecionaveis j1)) = Personagem vel Jogador (x,y) dir (c,l) esc ress vidas (pon+1) (n,z)
                                                                                                                |otherwise = p1

hitboxesColecionaveis :: Jogo -> [Hitbox]
hitboxesColecionaveis (Jogo _ _ lc _) = case lc of
  [] -> []
  ((col, (x, y)):xs) -> map hitboxColecionavel lc
    where
      hitboxColecionavel :: (Colecionavel, Posicao) -> Hitbox
      hitboxColecionavel (_, (x, y)) = ((x - 0.5, y - 0.5), (x + 0.5, y + 0.5))

morteInimigo :: Personagem -> Picture
morteInimigo i1@(Personagem velocidade Fantasma pos dir (c,l) esc ress vidas pon (n,z)) | vidas == 0 = Blank