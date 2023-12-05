module Mapa where 

import LI12324


mapa1 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5)
        [[p, p, p, p, p, p, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[v, v, v, v, v, v, v, v, v, v]
        ,[p, p, a, p, v, v, p, p, p, p]
        ,[v, v, v, v, v, v, v, v, e, v]
        ,[v, v, e, v, v, v, v, v, e, v]
        ,[p, p, p, p, p, p, p, p, p, p]
        ]
        where   p = Plataforma
                v = Vazio
                a = Alcapao
                e = Escada

