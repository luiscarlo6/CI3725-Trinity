{
{-|
	Este m�dulo, desarrollado en Alex, implanta un Analizador
	Lexicogr�fico para la calculadora vista en clase. Sirve como
	ejemplo de uso tanto para Alex como para Haddock.

-}
module Lexer (
  AlexPosn(..),
  -- * Funciones exportadas.
  -- ** Analizador Lexicográfico.
  lexer
  ) where
import Tokens
}

%wrapper "posn"

$digito = 0-9           -- UN digito
$letra  = [a-zA-Z]      -- UNA letra
$alfanum = [a-zA-Z0-9]  -- UN digito o letra
@string = \"(($printable # [\"\\]) | \\[\"n\\])*\"
@numero  = $digito+(\.$digito+)?
@identificador = $letra[$alfanum \_]*

tokens :-
  --Espacios en blanco y comentarios
  $white+  ;
  \#.*     ;

  --palabras del lenguaje
  program  { \p s -> TkProgram   (getPos p) }  
  use      {  \p s -> TkUse      (getPos p) }
  in       { \p s -> TkIn        (getPos p) }
  function { \p s -> TkFunction  (getPos p) }
  return   { \p s -> TkReturn    (getPos p) }
  end      { \p s -> TkEnd       (getPos p) }
  \;       { \p s -> TkSemicolon (getPos p) }
  \:       { \p s -> TkColon     (getPos p) }
  \,       { \p s -> TkComma     (getPos p) }

   
  -- Brackets
  \[       { \p s -> TkLBracket (getPos p) }
  \]       { \p s -> TkRBracket (getPos p) }
  \{       { \p s -> TkLBrace   (getPos p) }
  \}       { \p s -> TkRBrace   (getPos p) }
  \(       { \p s -> TkLParen   (getPos p) }
  \)       { \p s -> TkRParen   (getPos p) }

  --Declaraciones
  number   { \p s -> TkNumber  (getPos p) }
  matrix   { \p s -> TkMatrix  (getPos p) }
  boolean  { \p s -> TkBoolean (getPos p) }
  row      { \p s -> TkRow     (getPos p) }
  col      { \p s -> TkCol     (getPos p) }
  set      { \p s -> TkSet     (getPos p) }
  \=       { \p s -> TkAsig    (getPos p) }

  --IO
  print    { \p s -> TkPrint (getPos p) }
  read     { \p s -> TkRead  (getPos p) }

  --Condicionales
  if       { \p s -> TkIf   (getPos p) }
  then     { \p s -> TkThen (getPos p) }
  else     { \p s -> TkElse (getPos p) }


  --ciclos
  while    { \p s -> TkWhile (getPos p) }
  for      { \p s -> TkFor   (getPos p) }
  do       { \p s -> TkDo    (getPos p) }

  --literales
  false    { \p s -> TkFalse (getPos p)                 }
  true     { \p s -> TkTrue  (getPos p)                 }
  @numero  { \p s -> TkNum   (getPos p) s (read s)      }
  @string  { \p s -> TkStr   (getPos p) $ tail (init s) }

  --operaciones numéricas
  \+       { \p s -> TkPlus  (getPos p) }
  \-       { \p s -> TkMinus (getPos p) }
  \*       { \p s -> TkMult  (getPos p) }
  \/       { \p s -> TkExDiv (getPos p) }
  \%       { \p s -> TkExMod (getPos p) }
  div      { \p s -> TkDiv   (getPos p) }
  mod      { \p s -> TkMod   (getPos p) }

  --Operaciones de matrices
  \'             { \p s -> TkTranspose (getPos p) }
  \.\+\.         { \p s -> TkCPlus     (getPos p) }
  \.\-\.         { \p s -> TkCMinus    (getPos p) }
  \.\*\.         { \p s -> TkCMult     (getPos p) }
  \.\/\.         { \p s -> TkCExDiv    (getPos p) }
  \.\%\.         { \p s -> TkCExMod    (getPos p) }
  \.div\.        { \p s -> TkCDiv      (getPos p) }
  \.mod\.        { \p s -> TkCMod      (getPos p) }

  --operaciones de booleanos
  not            { \p s -> TkNot  (getPos p) }
  \&             { \p s -> TkAnd  (getPos p) }
  \|             { \p s -> TkOr   (getPos p) }
  \=\=           { \p s -> TkEqT  (getPos p) }
  \/\=           { \p s -> TkNEqT (getPos p) }
  \>             { \p s -> TkGT   (getPos p) }
  \<             { \p s -> TkLT   (getPos p) }
  \>\=           { \p s -> TkGEqT (getPos p) }
  \<\=           { \p s -> TkLEqT (getPos p) }

  --Identificadores
  @identificador { \p s -> TkIdent (getPos p) s }

  --Error
  .              { \p s -> TkError (getPos p) s }

{
lexer :: String -> [Token] 
lexer s = alexScanTokens s


getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ f c) = (f,c)

}
