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
mapa1 = Mapa ((2, 9), Oeste) (0.5, 17)
        [[p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v]
        ,[p, p, a, p, v, p, p, p, p, p, p, a, p, v, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, e, v, v, v, v, v, v, v, v, e, v, v]
        ,[v, v, e, v, v, v, v, v, e, v, v, v, v, v, v, v, v, e, v, v]
        ,[p, p, p, a, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, e, v, e, v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, e, v, e, v, v, v, v, v, v, v, v, v, v]
        ,[p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p]
        ]
        where   p = Plataforma
                v = Vazio
                a = Alcapao
                e = Escada


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