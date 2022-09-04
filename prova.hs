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
    -- FABI print $ generaraCoppia 9 147454
    print $ generaraCoppia 9 147272 -- IO


