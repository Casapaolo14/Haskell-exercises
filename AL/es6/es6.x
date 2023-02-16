-- CASAGRANDE PAOLO - 147272 - ESERCIZIO 6 ANALIZZATORI LESSICALI 
-- 6) Selezionare in un file di testo le stringhe di caratteri che rappresentano in notazione binaria un numero multiplo di 3.
--    Solo in numeri multipli di 4 vengono stampati in uscita, separati da uno spazio, la restante parte del testo 
--    viene eliminata. Il controllo di divisibilità deve essere implementato usando opportune espressioni regolari, 
--    e non l’operazione di divisione-resto.

{
module Main (main) where
}

%wrapper "basic"
-- macro definitions

-- scanner definition

tokens :-
    
    (1(01*0)*10*)+ {\s -> show((multipleOf4 s))}
    $white      ;
    $printable  ;

{

multipleOf4 :: String -> String
multipleOf4 xs =
    if (((last xs) == '0') && ((last (init xs)) == '0'))
        then xs
    else []

main :: IO()
main = do
    f <- readFile "es6.txt"
    print (alexScanTokens f)
}