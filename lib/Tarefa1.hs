{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where
import LI12324

colisoesParede :: Mapa -> Personagem -> Bool -- | Função que verifica, simultaneamente, se um personagem se encontra nos limites do mapa e se colide com uma plataforma
colisoesParede m1@(Mapa _ _ mblocos) p1 = colisoesJogadorParede m1 p1 && colisaoBloco p1 (hitboxPlataformas (posicaoBloco mblocos))


colisoesJogadorParede :: Mapa -> Personagem -> Bool -- | Função que verifica se um personagem se encontra dentro dos limites do mapa
colisoesJogadorParede (Mapa _ _ (l1:ls)) p
                  |fst (snd (defineHitbox p)) >= fromIntegral (length l1) || fst (fst (defineHitbox p)) <= 0 || snd (snd (defineHitbox p)) >= 0 = True  --laterais e topo do mapa 
                  |otherwise = False

-- colisões entre personagens

defineHitbox :: Personagem -> Hitbox  -- | Função que, dada uma personagem, define a sua hitbox
defineHitbox p = (ci,cs) -- | Hitbox é definida pelo canto inferior esquerdo e canto superior direito
                    where ci = (fst (posicao p) - fst (tamanho p)/2 , snd (posicao p) - snd (tamanho p)/2)
                          cs = (fst (posicao p) + fst (tamanho p)/2 , snd (posicao p) + (snd (tamanho p)/2))

colisaoHitbox :: Hitbox -> Hitbox -> Bool -- | Função que verifica se duas hitboxes se sobrepõem
colisaoHitbox ((x1,y1),(w1,z1)) ((x2,y2),(w2,z2))
                                                      |((x1 >= x2 && x1 <= w2) || (w1 >= x2 && w1 <= w2)) && ((y1 >= y2 && y1 <= z2) || (z1 >= y2 && z1 <= z2)) = True
                                                      |otherwise = False

colisoesPersonagens :: Personagem -> Personagem -> Bool -- | utiliza as duas funções anteriores para verificar colisões entre personagens
colisoesPersonagens p1 p2 = colisaoHitbox h1 h2
                                where h1 = defineHitbox p1
                                      h2 = defineHitbox p2

-- colisões entre personagens e plataformas

posicaoBloco :: [[Bloco]] -> [(Posicao,Bloco)] -- | atribui a cada bloco da matriz uma posição
posicaoBloco mblocos = [((fromIntegral x, fromIntegral y),bloco) | (y,linhaBloco) <- zip [0..] mblocos, (x,bloco) <- zip [0..] linhaBloco]

hitboxPlat :: (Posicao,Bloco) -> Hitbox 
hitboxPlat ((x,y),Plataforma) = ((x-0.5,y-0.5),(x+0.5,y+0.5))
hitboxPlat (_,_) = ((0,0),(0,0))
hitboxPlataformas :: [(Posicao,Bloco)]-> [Hitbox]
hitboxPlataformas = map hitboxPlat

colisaoBloco :: Personagem -> [Hitbox] -> Bool
colisaoBloco p1@(Personagem _ _ (x,y) _ _ _ _ _ _ _) plataformas = any (==True) (map (colisaoHitbox (defineHitbox p1)) plataformas)

--colisões úteis para outras funções

escolheBloco :: Personagem -> Mapa -> Bloco -- | escolhe o bloco abaixo da personagem (útil para colisões com alçapão)
escolheBloco p1@(Personagem _ _ (x,y) _ (l,a) _ _ _ _ _) (Mapa _ _ mblocos) = (mblocos !! floor (y+a/2)) !! floor x

colisaoBlocoPersonagem :: Personagem -> Bloco -> Bool -- | verifica se a persongem está em cima de uma plataforma
colisaoBlocoPersonagem p1 b = colisaoHitbox (defineHitbox p1) (hitboxPlat ((fst (posicao p1),snd (posicao p1)+0.5),b)) 

escolheBlocoPos :: Posicao -> [[Bloco]] -> Bloco -- | função que, após receber uma posição e uma matriz retorna qual o tipo de bloco nesta posição da matriz
escolheBlocoPos  (x,y) matriz = (matriz !! floor y) !! floor x

colisaoPersonagemEscada :: Personagem -> Jogo -> Bool -- | verifica se o personagem está em escada
colisaoPersonagemEscada p1 j1@(Jogo (Mapa _ _ mblocos) _ _ _)
                                                |escolheBlocoPos (posicao p1) mblocos == Escada = True
                                                |otherwise = False
--versão com mapa em vez de jogo
colisaoPersonagemEscada2 :: Personagem -> Mapa -> Bool
colisaoPersonagemEscada2 p1 (Mapa _ _ mblocos)
                                                |escolheBlocoPos (posicao p1) mblocos == Escada = True
                                                |otherwise = False
