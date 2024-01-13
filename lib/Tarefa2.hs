{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1
import Data.List

valida :: Jogo -> Bool -- | aplica todas as funções da tarefa, retornando True se todas estas retornarem True, sendo deste modo o jogo considerado válido
valida jogo1@(Jogo mapa1 inimigos colecionaveis jogador ) =
     validaMapa mapa1 && validaRessalto (jogador:inimigos) && validaPos jogador inimigos && validaNumP inimigos && validaVidaFantasma inimigos && validaEscadas mapa1 && validaAl jogo1 && validaColecionaveis jogo1 && validaPosPersonagem mapa1 (jogador:inimigos)

--1

validaMapa :: Mapa -> Bool -- | verifica se o mapa tem chão
validaMapa (Mapa _ _ matriz) = all (== Plataforma) (last matriz) -- | Verficar se a última linha do mapa é constituida por elementos do tipo "Plataforma" e se a posição inicial é diferente da final

--2

validaRessalto :: [Personagem] -> Bool -- | verifica se os inimigos têm a propriedade ressalta True e o jogador False
validaRessalto lp@(p:ps) = all aux lp
                        where aux p = if tipo p == Jogador then not (ressalta p) else ressalta p
--3

validaPos :: Personagem -> [Personagem] -> Bool -- | verifica se a hitbox do jogador colide com a hitbox de algum dos inimigos. Se tal acontecer a função retorna False, indicando que o jogo não é válido.
validaPos p1 p2 = not (any (== True) (map (colisaoHitbox h1) h2))
                where h1 = defineHitbox p1
                      h2 = map defineHitbox p2
--4

validaNumP :: [Personagem] -> Bool -- | verifica se o número de inimigos é igual ou superior a 2
validaNumP p = length p >= 2

--5

validaVidaFantasma :: [Personagem] -> Bool -- | verifica se os fantasmas possuem apenas uma vida
validaVidaFantasma i@(i1:xs) = not (any (==False) (map verificavida i))
                              where verificavida :: Personagem -> Bool
                                    verificavida i = vida i == 1
--6

validaEscada :: (Posicao,Bloco) -> [[Bloco]]  -> Bool -- | verifica se a escada possui uma plataforma abaixo ou acima e que não começa nem terminam num alçapão
validaEscada ((x,y),b1) mblocos
                              |b1== Escada = (cima == Plataforma || baixo == Plataforma) && (cima /= Alcapao) && (baixo/= Alcapao)
                              |otherwise = True
                                                  where     cima = escolheBlocoPos  (x,y+1) mblocos
                                                            baixo = escolheBlocoPos (x,y-1) mblocos
validaEscadas :: Mapa -> Bool -- | aplica a função anterior a todas as escadas do mapa
validaEscadas (Mapa _ _ mblocos) = all (`validaEscada` mblocos) (posicaoBloco mblocos)

--7

validaAl::Jogo -> Bool -- | verifica se o tamanho de um jogador é menor do que a largura do alçapão (1)
validaAl j1 = fst (tamanho (jogador j1)) <= largAl
               where largAl = 1
--8

validaColecionaveis :: Jogo -> Bool -- | extrai a posição de cada colecionável e verifica, com recurso à função blocoNaPosicao, se o bloco nessa posição é vazio, retornando True se tal acontecer para todos os colecionáveis
validaColecionaveis  (Jogo (Mapa _ _ matriz) _ listac _ ) =
     all (\(colecionavel1, (xc,yc)) ->  escolheBlocoPos  (xc,yc) matriz == Vazio) listac

validaPosPersonagem :: Mapa -> [Personagem] -> Bool -- | verifica se as personagens se encontram em blocos do tipo Vazio
validaPosPersonagem  m1@(Mapa _ _ matriz) ((Personagem _ _ (x,y) _ (l,a) _ _ _ _ _):ps) = (((escolheBlocoPos  (x,y+a/2) matriz) == Vazio) && validaPosPersonagem m1 ps)