{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import LI12324
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

--atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
--atualiza ações ação jogo1 = jogo1 {
--      jogador = ,
--      inimigos = 
--}

eventosAçao :: Event -> Maybe Acao
eventosAçao (EventKey (SpecialKey KeySpace) Down _ _) = Just Saltar
eventosAçao (EventKey (SpecialKey KeyUp)Down _ _) = Just Subir
eventosAçao (EventKey (SpecialKey KeyRight) Down _ _) = Just AndarDireita
eventosAçao (EventKey (SpecialKey KeyLeft)Down _ _) = Just AndarEsquerda
eventosAçao (EventKey (SpecialKey KeyDown) Down _ _) = Just Descer
eventosAçao _ = Nothing

listaEventos :: [Event] -> [Maybe Acao]
listaEventos = map eventosAçao