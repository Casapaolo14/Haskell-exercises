-- CASAGRANDE PAOLO - 147272 - ESERCIZIO 1 ANALIZZATORI LESSICALI 
-- 1) Riconoscere in un file di testo le seguenti classi di lessemi
--      - Parole chiave: var, function, procedure, while, do, if, then, else, for
--      - Identificatori: stringhe che iniziano con una lettere minuscola dell’alfabeto seguite da lettere, cifre, e i simboli ’_‘,’-’.
--      - Costanti numeriche: sequenze di cifre
--      - Operatori: “+”, “++”, “-”, “–”, “=”, “==”
--    Per ogni lessema riconosciuto, stampare una coppia (classe, valore).

{
module Main (main) where
}

%wrapper "basic"

-- macro definitions
$min = [a-z] -- Lettere Minuscole
    $lett = [a-zA-Z]
    $cifr = [0-9]
    $sym = ['\_' '\-'] 

$const = [0-9] -- Costanti numeriche

$plus = [\+] -- Operatori
$minus = [\-] -- Operatori
$equal = [\=] -- Operatori

-- scanner definition

tokens :-
    
    var | procedure | if | then | else | while | procedure | do | for {\s -> "(Parola chiave, "++ s ++")"}
    ($min$lett*$cifr*$sym*) {\s -> "(Identificatore, " ++ s ++ ")"}
    $const+   {\s -> ("(Costante numerica, "++ s ++")")}
    ($plus) | ($plus$plus) | ($minus) | ($minus$minus) | ($equal) | ($equal$equal) {\s -> ("(Operatore, " ++ s ++ ")")}
    $white      ;
    $printable  ;

{
main :: IO()
main = do
    f <- readFile "es1.txt"
    print (alexScanTokens f)
}