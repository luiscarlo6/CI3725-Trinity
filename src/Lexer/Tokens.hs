{-| 
  Module      : Tokens
  Copyright   : Universidad Simón Bolívar
  Maintainer  : Luiscarlo Rivera (09-11020) 
	        & Traductores e Interpretadores (CI-3725) 

  Módulo que contiene la definición del tipo @Tokens@, con sus
  constructores, según la especificación del lenjuaje @Trinity@.
-}
module Tokens (
  -- * Tipos exportados
  -- ** Tokens
  Token (..)
  ) where
{- El tipo de datos @Token@ modela los diferentes /Tokens/ que se pueden 
   encontrar en el lenguaje 2Trinity. Todos los /Tokens/ contienen información
   acerca de la fila y columna donde fué encontrado, en forma de tupla (f,c).
   En los casos en que sea necesaria información extra - por ejemplo TkNum, del
   cual nos interesa saber además de su posición, cuál es el número que se 
   representa (Lo mismo pasa con el String) - se añade un parámetro extra para 
   su almacenamiento.

   El tipo de datos @Token@ se declara derivando de @Show@ para que
   se pueda probar el Analizador Lexicográfico individualmente, puesto
   que al invocar la función @yylex@ la lista producida será presentada
   directamente en pantalla.

   El tipo de datos @Token@ Se declara derivando de @Eq@ y @Show@ para facilitar el uso de
   este tipo de datos en futuras implementaciones que lo utilicen.
-}
data Token =
  -- Palabras reservadas del Lenguaje
  TkProgram   |
  TkUse       |
  TkNumber    | 
  TkIn        |
  TkPrint     |
  TkRead      |
  TkIf        |
  TkThen      |
  TkElse      |
  TkEnd       |
  TkMatrix    |
  TkBoolean   |
  TkSet       |
  TkWhile     |
  TkFor       |
  TkDo        |
  TkNot       |
  TkFalse     |
  TkTrue      |
  TkRow       |
  TkCol       |
  TkDiv       | 
  TkMod       |
  TkFunction  |
  TkReturn    | 
  -- Operadores y elementos sintácticos
  TkSemicolon |
  TkColon     |
  TkQuote     |
  TkBackslash |
  TkAsig      |
  TkEqT       |
  TkNEqT      |
  TkGT        |
  TkLT        |
  TkGEqT      |
  TkLEqT      |
  TkComma     |
  TkLBracket  |
  TkRBracket  |
  TkLBrace    |
  TkRBrace    |
  TkRParen    |
  TkLParen    |
  TkAnd       |
  TkOr        |  
  TkPlus      |
  TkMinus     |
  TkMult      |
  TkTranspose |
  -- Tentativos, operadores cruzados
  TkCPlus     |
  TkCMinus    |
  TkCMult     |
  TkCDiv      |
  TkCMod      |
  -- Los que necesitan guardar cosas
  TkIdent       
  deriving(Eq,Show)

