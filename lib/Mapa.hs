{-|
Module      : Mapa
Description : Define os mapas 
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para o mapa do projeto de LI1 em 2023/24.
-}
module Mapa where 

import LI12324

-- | Estrutura do primeiro mapa
mapa1 = Mapa ((2, 8), Oeste) (18, 2)
        [[p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, p, v]
        ,[p, p, a, p, v, p, p, p, p, p, p, a, p, v, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, e, v, v, v, v, v, v, v, v, e, v, v]
        ,[v, v, v, v, v, v, v, v, e, v, v, v, v, v, v, v, v, e, v, v]
        ,[p, p, p, a, p, p, p, p, p, p, p, p, p, p, v, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, e, v, e, v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, e, v, e, v, v, v, v, v, v, v, v, v, v]
        ,[p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p]
        ]
        where   p = Plataforma
                v = Vazio
                a = Alcapao
                e = Escada

inimigo1 = Personagem (0,0) Fantasma (5,3) Este (1,1) False True 1 0 (False,0)

inimigo2 = Personagem (0,0) Fantasma (6,5.5) Oeste (1,1) False True 1 0 (False,0)

inimigo3 = Personagem (0,0) MacacoMalvado (2,2.7) Oeste (1,1) False True 1 0 (False,0)

jogador1 = Personagem (0,0) Jogador (2,8) Oeste (0.5,0.5) False False 3 0 (False,0)


-- | Estrutura do segundo mapa 
mapa2 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
        [[p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, v, v]       
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[p, p, p, p, p, p, p, p, p, p]
        ]
        where   p = Plataforma
                v = Vazio
                a = Alcapao
                e = Escada