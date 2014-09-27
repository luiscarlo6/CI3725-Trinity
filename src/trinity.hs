{-| 
  Module      : Main
  Copyright   : Universidad Simón Bolívar
  Maintainer  : Luiscarlo Rivera (09-11020) 
	        & Traductores e Interpretadores (CI-3725) 
                Entrega: Proyecto # 1
  Módulo que implementa el programa principal del Analizador Lexicografico para el lenguaje @Trinity@.
-}
module Main (
  -- * Función Principal.
  main
) where

import System.IO
import System.Environment
import Lexer
import Tokens

{-|
   Función principal.

   Debe ser invocado de la forma: ./trinity <Archivo_de_entrada>
 -}
main :: IO ()
main =
  do
    args <- getArgs
    if length args /= 1
      then error "\nError: El programa debe ejecutarse con un argumento de la forma: ./triniy <Archivo_de_entrada>\n"
      else do
      contents <- readFile $ head args
      putStr $ unlines $ map show $ lexer contents
