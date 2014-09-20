{
{-|
	Este m�dulo, desarrollado en Alex, implanta un Analizador
	Lexicogr�fico para la calculadora vista en clase. Sirve como
	ejemplo de uso tanto para Alex como para Haddock.

-}
module Lexer (
  -- * Funciones exportadas.
  -- ** Analizador Lexicográfico.
  lexer
  ) where
import Tokens
}

%wrapper "basic"

$digito = 0-9           -- UN digito
$letra  = [a-zA-Z]      -- UNA letra

tokens :-

  $white+                        ;
  program                        { \s -> TkProgram }
{
lexer :: String -> [Token] 
lexer s = alexScanTokens s
}
