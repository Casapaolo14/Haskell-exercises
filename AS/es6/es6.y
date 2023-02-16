-- CASAGRANDE PAOLO - 147272 - ESERCIZIO 6 ANALIZZATORI SINTATTICI 
-- 6) Sequenze di espressioni in linguaggio Haskell formate da:
--      - Numeri interi
--      - Identificatori 
--      - Applicazione
--      - Costrutti let in e case
--    L’analizzatore deve restituire gli alberi sintattici delle espressioni ricevute in ingresso.

{
module Main where
import HaskellLexer
import Data.Char
}

%name hsScanner
%tokentype { Token }
%error { parseError }

-- Dichiarazione del tipo di token

%token

    var                  {TokenVar $$}
    int                 {TokenInt $$}
    'let'               {TokenLet}
    'in'                {TokenIn}
    'case'              {TokenCase}
    'of'                {TokenOf}
    '='                 {TokenEq}
    fun              {TokenFun $$}

-- Definizione della grammatica
%%
Exp 
    : 'let' var '=' Exp 'in' Exp	{ Let $2 $4 $6 }
    | fun var '=' 'case' var 'of'   { Case $1 $2 }
    | Factor                    { Factor $1 }

Factor 
    : int	    		{ Int $1 }
 	| var	    		{ Var $1 }

{
-- Gestione degli errori di sintassi
parseError :: [Token] -> a
parseError _ = error ("Parse error!")

-- Definizione del tipo di dati per gli alberi sintattici
data Exp
    = Let String Exp Exp
    | Case String String
    | Factor Factor
    deriving (Eq, Show)

data Factor
    = Int Int
    | Var String
    | Brack Exp
    deriving (Eq, Show)

data Token
    = TokenLet
    | TokenIn
    | TokenInt Int
    | TokenVar String
    | TokenFun
    | TokenCase
    | TokenOf
    | TokenEq
    deriving (Eq, Show)


main :: IO ()
main = do 
    s <- readFile "code.hs"
    print ( hsScanner (alexScanTokens s))

}