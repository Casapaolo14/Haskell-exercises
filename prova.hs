import Data.List
import System.IO

getString :: Double -> Double -> String

getString hits bats
    | avg < 0.200 = "merda"
    | avg < 0.400 = "buono"
    | avg < 0.600 = "ottimo"
    | otherwise = "cazzp"
    where avg = hits / bats

generaraCoppia nEsercizi matricola = (primo, secondo) where 
  primo = matricola `mod` nEsercizi + 1
  secondo = (matricola `mod` (nEsercizi - 3) + primo + 1) `mod`  nEsercizi + 1


main :: IO ()
main = do
    --putStrLn "Hello, World!"

    -- Lessicali e Sintattici
    print $ generaraCoppia 9 147272 -- IO

-- 20 es, almeno due per capitolo


-- esercizi -> (6,1)
-- ANALIZZATORE LESSICALE

-- 1) Riconoscere in un file di testo le seguenti classi di lessemi
--      - Parole chiave: var, function, procedure, while, do, if then, else, for
--      - Identificatori: stringhe che iniziano con una lettere minuscola dell’alfabeto seguite da lettere, cifre, e i simboli ’_‘,’-’.
--      - Costanti numeriche: sequenze di cifre, operatori: “+”, “++”, “-”, “–”, “=”, “==”
--    Per ogni lessema riconosciuto, stampare una coppia (classe, valore).

-- 6) Selezionare in un file di testo le stringhe di caratteri che rappresentano in notazione binaria un numero multiplo di 3.
--    Solo in numeri multipli di 4 vengono stampati in uscita, separati da uno spazio, la restante parte del testo 
--    viene eliminata. Il controllo di divisibilità deve essere implementato usando opportune espressioni regolari, 
--    e non l’operazione di divisione-resto.

-- ANALIZZATORE SINTATTICO
-- 1) Il linguaggio formato da espressioni aritmetiche scritte in notazione poloacca inversa e costruite
--    a partire dalle costanti intere e le 4 operazioni aritmetiche. 
--    L’analizzatore deve restituire l’albero della struttura sintattica dell’espressione ricevuta in ingresso.

-- 6) Sequenze di espressioni in linguaggio Haskell formate da:
--      - Numeri interi
--      - Identificatori
--      - Applicazione
--      - Costrutti let in e case
--    L’analizzatore deve restituire gli alberi sintattici delle espressioni ricevute in ingresso.


