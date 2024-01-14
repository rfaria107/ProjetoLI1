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
      jogador =  moveJogador (desarmaJogador (aplicarDanoJogador (aplicarGravidade (pontosApanharMoeda  (apanharMartelos (jogador jogo1) jogo1) jogo1) (mapa jogo1)) (inimigos jogo1)) tempo) (mapa jogo1),
      inimigos = map (aplicarDanoMartelo (jogador jogo1)) (map (morteInimigo) (inimigos jogo1))
}

--1

hitboxDano :: Personagem -> Hitbox -- a Hitbox do dano de um jogador é uma hitbox do seu tamanho colocada a seu lado para onde este estiver virado com o martelo.
hitboxDano p@(Personagem _ _ _ dir _ _ _ _ _ _) | dir == Oeste = ((x1-l,y1),(x2-l,y2))
                                                | dir == Este  = ((x1+l,y1),(x2+l,y2))
                                                | otherwise = ((x1,y1),(x2,y2))    
                                                                        where l = fst (tamanho p)
                                                                              ((x1,y1),(x2,y2)) = defineHitbox p
aplicarDanoMartelo :: Personagem -> Personagem -> Personagem -- a p1 é o jogador com a propriedade "aplicaDano" a True, a p2 é um Fantasma. Se a hitbox do dano colidir com a hitbox do fantasma, este perde 1 vida.
aplicarDanoMartelo p1@(Personagem _ Jogador _ _ _ _ _ _ _ (False,_)) p2@(Personagem velocidade tipo pos dir (c,l) esc ress vidas pon (n,z)) = p2
aplicarDanoMartelo p1@(Personagem _ Jogador _ _ _ _ _ _ _ (True,_)) p2@(Personagem velocidade tipo pos dir (c,l) esc ress vidas pon (n,z))
      |colisaoHitbox (hitboxDano p1) (defineHitbox p2) && tipo == Fantasma = Personagem velocidade Fantasma pos dir (c,l) esc ress (vidas-1) pon (n,z)
      |otherwise = p2
--2

morteInimigo :: Personagem -> Personagem -- envia o inimigo para uma posição fora da matriz de modo que este não interfira no resto do jogo.
morteInimigo i1@(Personagem velocidade tipo pos dir (c,l) esc ress vidas pon (n,z))   |tipo == Fantasma && vidas == 0 = Personagem velocidade Fantasma (-10,-10) dir (c,l) esc ress vidas pon (n,z)
                                                                                      |otherwise = i1
--3
                                                                                                                                                      
aplicarGravidade :: Personagem -> Mapa -> Personagem
aplicarGravidade p1@(Personagem (vx,vy) Jogador pos dir (c,l) esc ress vidas pon (n,z)) m1@(Mapa _ _ mblocos)     |escolheBloco p1 m1 == Vazio || escolheBloco p1 m1 == Escada && not (colisaoPersonagemEscada2 p1 m1)  = (Personagem (vx,vy+(0.01)) Jogador pos dir (c,l) esc ress vidas pon (n,z))
                                                                                                                  |otherwise = p1
--4

aplicarDanoJogador :: Personagem -> [Personagem] -> Personagem -- p1 é o jogador e p2 o fantasma. Se a hitbox de ambos colidir o jogador perde uma vida.
aplicarDanoJogador p1 [] = p1
aplicarDanoJogador p1@(Personagem vel Jogador pos dir (c,l) esc ress vidas pon (n,z)) (p2@(Personagem velocidade2 _ pos2 dir2 (c2,l2) esc2 ress2 vidas2 pon2 (n2,z2)):xs)   |colisaoHitbox (defineHitbox p1) (defineHitbox p2) = (Personagem vel Jogador pos dir (c,l) esc ress (vidas-1) pon (n,z))
                                                                                                                                                                            |otherwise = aplicarDanoJogador p1 xs
--5

pontosApanharMoeda :: Personagem -> Jogo -> Personagem
pontosApanharMoeda p1@(Personagem vel Jogador (x,y) dir (c,l) esc ress vidas pon (n,z)) j1@(Jogo _ _ col _)     |any (==True) (map (colisaoHitbox (defineHitbox p1)) (hitboxesMoedas j1)) = Personagem vel Jogador (x,y) dir (c,l) esc ress vidas (pon+1) (n,z)
                                                                                                                |otherwise = p1

apanharMartelos :: Personagem -> Jogo -> Personagem
apanharMartelos p1@(Personagem vel Jogador (x,y) dir (c,l) esc ress vidas pon (n,z)) j1@(Jogo _ _ col _)        |any (==True) (map (colisaoHitbox (defineHitbox p1)) (hitboxesMartelos j1)) = Personagem vel Jogador (x,y) dir (c,l) esc ress vidas pon (True,10)
                                                                                                                |otherwise = p1
                                                                                                                                                                                                                                
desapareceColecionavel:: Personagem -> Jogo -> Jogo
desapareceColecionavel p1 jogo1@(Jogo m i lc@((col,(x,y)):xs) j)        | colisaoHitbox (defineHitbox p1) (hitboxColecionavel (col,(x,y))) = desapareceColecionavel p1 (Jogo m i ((col, (-10,-10)):xs) j) 
                                                                        | otherwise = desapareceColecionavel p1 (Jogo m i lc j)

hitboxesColecionaveis :: Jogo -> [Hitbox]
hitboxesColecionaveis (Jogo _ _ lc _) = case lc of
  [] -> []
  ((col, (x, y)):xs) -> map hitboxColecionavel lc
    
hitboxColecionavel :: (Colecionavel, Posicao) -> Hitbox
hitboxColecionavel (_, (x, y)) = ((x - 0.5, y - 0.5), (x + 0.5, y + 0.5))

hitboxesMartelos :: Jogo -> [Hitbox]
hitboxesMartelos (Jogo _ _ lc _) = case lc of
  [] -> []
  ((Moeda, (x, y)):xs) -> map hitboxColecionavel xs
  ((Martelo, (x, y)):xs) -> map hitboxColecionavel lc

hitboxMartelo :: (Colecionavel, Posicao) -> Hitbox
hitboxMartelo (Martelo, (x, y)) = ((x - 0.5, y - 0.5), (x + 0.5, y + 0.5))

hitboxesMoedas :: Jogo -> [Hitbox]
hitboxesMoedas (Jogo _ _ lc _) = case lc of
  [] -> []
  ((Martelo, (x, y)):xs) -> map hitboxColecionavel xs
  ((Moeda, (x, y)):xs) -> map hitboxColecionavel lc

hitboxMoeda :: (Colecionavel, Posicao) -> Hitbox
hitboxMoeda (Moeda, (x, y)) = ((x - 0.5, y - 0.5), (x + 0.5, y + 0.5))

--6

--pisaAlcapao :: Personagem -> Jogo ->  Bool
--pisaAlcapao p1 j1@(Jogo mapa1 _ _ _)
--                                                |escolheBloco p1 mapa1 == Alcapao = True
--                                                |otherwise = False

--desapareceAlcapao :: Personagem -> Jogo -> Mapa
--desapareceAlcapao p1 j1@(Jogo mapa1@(Mapa _ _ ((x:xs))) _ _ _) |pisaAlcapao p1 j1 == True =

--transformaBlocos :: Posicao -> Bloco -> [[Bloco]] -> [[Bloco]]
--transformaBlocos pos bloco mblocos@[blocos:bs]  |bloco==Alcapao && pos == fst posicaoBloco mblocos = [[(Vazio:bs)]]

--movimentar o jogador

moveJogador :: Personagem ->  Mapa -> Personagem
moveJogador j1@(Personagem (vx,vy) Jogador (x,y) dir (c,l) esc ress vidas pon (n,z)) m1
                                                                                |vx>0 = Personagem (vx,vy) Jogador (x+0.05,y) dir (c,l) esc ress vidas pon (n,z)
                                                                                |vx<0 = Personagem (vx,vy) Jogador (x-0.05,y) dir (c,l) esc ress vidas pon (n,z)
                                                                                |vy>0 = Personagem (vx,0) Jogador (x,y+0.05) dir (c,l) esc ress vidas pon (n,z)
                                                                                |vy<0 = Personagem (vx,vy) Jogador (x,y-0.05) dir (c,l) esc ress vidas pon (n,z)
                                                                                |otherwise = j1
desarmaJogador :: Personagem -> Tempo -> Personagem
desarmaJogador p1@(Personagem (vx,vy) Jogador (x,y) dir (c,l) esc ress vidas pon (dano,trestante)) t | dano && trestante >0 = (Personagem (vx,vy) Jogador (x,y) dir (c,l) esc ress vidas pon (dano,trestante-1))
                                                                                                     | dano && trestante == 0 =  (Personagem (vx,vy) Jogador (x,y) dir (c,l) esc ress vidas pon (False,0))
                                                                                                     | otherwise = p1
