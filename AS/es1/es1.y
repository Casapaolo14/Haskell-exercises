-- CASAGRANDE PAOLO - 147272 - ESERCIZIO 6 ANALIZZATORI SINTATTICI 
-- 1) Il linguaggio formato da espressioni aritmetiche scritte in notazione polacca inversa e costruite
--    a partire dalle costanti intere e le 4 operazioni aritmetiche. 
--    L’analizzatore deve restituire l’albero della struttura sintattica dell’espressione ricevuta in ingresso.

{
module Main where
import RPNLexer
}

%name rpnScanner
%tokentype { Token }
%error { parseError }

%token
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenTimes }
    '/'         { TokenDivide }
    int         { TokenInt $$ }

%%

Exp   : Exp Exp '+'             { $1 $3 Plus }
      | Exp Exp '-'            { $1 $3 Minus }
      | Exp Exp '*'             { $1 $3 Times }
      | Exp Exp '/'             { $1 $3 Div }
      | int                     { Int $1 }


{
-- Gestione degli errori di sintassi
parseError :: [Token] -> a
parseError _ = error ("Parse error!")

main :: IO ()
main = do 
    s <- readFile "RPN.txt"
    print (rpnScanner(alexScanTokens s))

}

