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
  TkProgram   (Int,Int)|
  TkUse       (Int,Int)|
  TkNumber    (Int,Int)| 
  TkIn        (Int,Int)|
  TkPrint     (Int,Int)|
  TkRead      (Int,Int)|
  TkIf        (Int,Int)|
  TkThen      (Int,Int)|
  TkElse      (Int,Int)|
  TkEnd       (Int,Int)|
  TkMatrix    (Int,Int)|
  TkBoolean   (Int,Int)|
  TkSet       (Int,Int)|
  TkWhile     (Int,Int)|
  TkFor       (Int,Int)|
  TkDo        (Int,Int)|
  TkNot       (Int,Int)|
  TkFalse     (Int,Int)|
  TkTrue      (Int,Int)|
  TkRow       (Int,Int)|
  TkCol       (Int,Int)|
  TkDiv       (Int,Int)|
  TkMod       (Int,Int)|
  TkFunction  (Int,Int)|
  TkReturn    (Int,Int)| 
  -- Operadores y elementos sintácticos
  TkSemicolon (Int,Int)|
  TkColon     (Int,Int)|
  TkAsig      (Int,Int)|
  TkEqT       (Int,Int)|
  TkNEqT      (Int,Int)|
  TkGT        (Int,Int)|
  TkLT        (Int,Int)|
  TkGEqT      (Int,Int)|
  TkLEqT      (Int,Int)|
  TkComma     (Int,Int)|
  TkLBracket  (Int,Int)|
  TkRBracket  (Int,Int)|
  TkLBrace    (Int,Int)|
  TkRBrace    (Int,Int)|
  TkRParen    (Int,Int)|
  TkLParen    (Int,Int)|
  TkAnd       (Int,Int)|
  TkOr        (Int,Int)|  
  TkPlus      (Int,Int)|
  TkMinus     (Int,Int)|
  TkMult      (Int,Int)|
  TkExDiv     (Int,Int)|
  TkExMod     (Int,Int)|
  TkTranspose (Int,Int)|
  -- Tentativos, operadores cruzados
  TkCPlus     (Int,Int)|
  TkCMinus    (Int,Int)|
  TkCMult     (Int,Int)|
  TkCExDiv    (Int,Int)|
  TkCExMod    (Int,Int)|
  TkCDiv      (Int,Int)|
  TkCMod      (Int,Int)|
  
  TkIdent     (Int,Int) String       |
  TkNum       (Int,Int) String Float |
  TkStr       (Int,Int) String       |
  TkError     (Int,Int) String
  
  deriving(Eq)



instance Show Token where
  show tok =
    case tok of
     TkProgram   (f,c)     -> format f c "Palabra reservada: program"
     TkUse       (f,c)     -> format f c "Palabra reservada: use" 
     TkNumber    (f,c)     -> format f c "Palabra reservada: number"
     TkIn        (f,c)     -> format f c "Palabra reservada: in"
     TkPrint     (f,c)     -> format f c "Palabra reservada: print"
     TkRead      (f,c)     -> format f c "Palabra reservada: read"
     TkIf        (f,c)     -> format f c "Palabra reservada: if"
     TkThen      (f,c)     -> format f c "Palabra reservada: then"
     TkElse      (f,c)     -> format f c "Palabra reservada: else"
     TkEnd       (f,c)     -> format f c "Palabra reservada: end"
     TkMatrix    (f,c)     -> format f c "Palabra reservada: matrix"
     TkBoolean   (f,c)     -> format f c "Palabra reservada: boolean"
     TkSet       (f,c)     -> format f c "Palabra reservada: set"
     TkWhile     (f,c)     -> format f c "Palabra reservada: while"
     TkFor       (f,c)     -> format f c "Palabra reservada: for"
     TkDo        (f,c)     -> format f c "Palabra reservada: do"
     TkNot       (f,c)     -> format f c "Palabra reservada: not"
     TkFalse     (f,c)     -> format f c "Palabra reservada: false"
     TkTrue      (f,c)     -> format f c "Palabra reservada: true"
     TkRow       (f,c)     -> format f c "Palabra reservada: row"
     TkCol       (f,c)     -> format f c "Palabra reservada: col"
     TkFunction  (f,c)     -> format f c "Palabra reservada: function"
     TkReturn    (f,c)     -> format f c "Palabra reservada: return" 
     -- Operadores y elementos sintácticos
     TkSemicolon (f,c)     -> format f c "Separador de instrucciones \";\""
     TkColon     (f,c)     -> format f c "Separador de filas \":\""
     TkComma     (f,c)     -> format f c "Separador de columnas \",\""
     TkAsig      (f,c)     -> format f c "Asignación \"=\""
     TkEqT       (f,c)     -> format f c "Comparador de igualdad \"=\""
     TkNEqT      (f,c)     -> format f c "Comparador de desigualdad \"/=\""
     TkGT        (f,c)     -> format f c "Comparador de mayor que \">\""
     TkLT        (f,c)     -> format f c "Comparador de menor que \"<\""
     TkGEqT      (f,c)     -> format f c "Comparador de mayor o igual que \">=\""
     TkLEqT      (f,c)     -> format f c "Comparador de menor o igual que \"<=\""
     TkLBracket  (f,c)     -> format f c "Abridor corchete \"[\""
     TkRBracket  (f,c)     -> format f c "Cerrador corchete \"]\""
     TkLBrace    (f,c)     -> format f c "Abridor llave \"{\""
     TkRBrace    (f,c)     -> format f c "Cerrador llave \"}\""
     TkLParen    (f,c)     -> format f c "Abridor paréntesis \"(\""
     TkRParen    (f,c)     -> format f c "Cerrador paréntesis \")\""
     TkAnd       (f,c)     -> format f c "Conjunción \"&\""
     TkOr        (f,c)     -> format f c "Disyunción \"|\""  
     TkPlus      (f,c)     -> format f c "Suma \"+\""
     TkMinus     (f,c)     -> format f c "Resta o inverso aditivo \"-\""
     TkMult      (f,c)     -> format f c "Producto \"*\""
     TkDiv       (f,c)     -> format f c "División entera \"div\""
     TkMod       (f,c)     -> format f c "Resto entero de la división \"mod\""
     TkExDiv     (f,c)     -> format f c "División exacta \"/\""
     TkExMod     (f,c)     -> format f c "Resto exacto de la división \"%\""
     TkTranspose (f,c)     -> format f c "Transpuesta de una matriz \"'\""
     -- Tentativos, operadores cruzados
     TkCPlus     (f,c)     -> format f c "Operador cruzado suma \".+.\""
     TkCMinus    (f,c)     -> format f c "Operador cruzado resta o inverso aditivo \".-.\""
     TkCMult     (f,c)     -> format f c "Operador cruzado producto \".*.\""
     TkCExDiv    (f,c)     -> format f c "Operador cruzado división exacta \"./.\""
     TkCExMod    (f,c)     -> format f c "Operador cruzado resto exacto de la división \".%.\""
     TkCDiv      (f,c)     -> format f c "Operador cruzado división entera \".div.\""
     TkCMod      (f,c)     -> format f c "Operador cruzado resto entero de la división \".mod.\""
     TkIdent     (f,c) s   -> format f c $ "Identificador: "++s
     TkNum       (f,c) s _ -> format f c $ "Literal numérico: "++s
     TkStr       (f,c) s   -> format f c $ "Literal de cadena de caracteres: \""++s++"\""
     TkError     (f,c) s   -> format f c $ "Error caracter inesperado: "++s

format :: Int -> Int -> String -> String
format f c s = s ++ ", Fila:"++(show f)++", Columna:"++(show c)++".\n"
