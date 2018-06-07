import Data.List
import Data.Map


data Zdanie = Z Char | N Zdanie | K Zdanie Zdanie | A Zdanie Zdanie | C Zdanie Zdanie

drukuj :: Zdanie -> String
drukuj (Z chr) = [chr]
drukuj (N zdanie) = "~" ++ (drukuj zdanie)
drukuj (K zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " & " ++ (drukuj zdanie2) ++ ")"
drukuj (A zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " | " ++ (drukuj zdanie2) ++ ")"
drukuj (C zdanie1 zdanie2) = "(" ++ (drukuj zdanie1) ++ " => " ++ (drukuj zdanie2) ++ ")"

quicksort :: [Char] -> [Char]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

unique :: [Char] -> [Char]
unique [chr] = [chr]
unique (x:xs) = if x == head xs then unique xs else [x] ++ unique xs

przecinki :: [Char] -> [Char]
przecinki [] = []
przecinki [chr] = [chr]
przecinki (x:xs) = [x] ++ ", " ++ przecinki xs

wydobadz_zmienne :: Zdanie -> [Char]
wydobadz_zmienne (Z chr) = [chr]
wydobadz_zmienne (N zdanie) = wydobadz_zmienne zdanie
wydobadz_zmienne (K zdanie1 zdanie2) = (wydobadz_zmienne zdanie1) ++ (wydobadz_zmienne zdanie2)
wydobadz_zmienne (A zdanie1 zdanie2) = (wydobadz_zmienne zdanie1) ++ (wydobadz_zmienne zdanie2)
wydobadz_zmienne (C zdanie1 zdanie2) = (wydobadz_zmienne zdanie1) ++ (wydobadz_zmienne zdanie2)

wypisz_zmienne :: Zdanie -> IO()
wypisz_zmienne zdanie = putStrLn $ "[" ++ (przecinki $ unique $ quicksort $ wydobadz_zmienne zdanie) ++ "]"

sprawdz :: Zdanie -> Map Char Bool -> Bool
sprawdz (Z chr) (mapa) = if mapa ! chr == True then True else False
sprawdz (N zdanie) (mapa) = not $ sprawdz zdanie mapa
sprawdz (K zdanie1 zdanie2) (mapa) = (sprawdz zdanie1 mapa) && (sprawdz zdanie2 mapa)
sprawdz (A zdanie1 zdanie2) (mapa) = (sprawdz zdanie1 mapa) || (sprawdz zdanie2 mapa)
sprawdz (C zdanie1 zdanie2) (mapa) = if (sprawdz zdanie1 mapa) == True && (sprawdz zdanie2 mapa) == False then
                                    False else True

lista_false :: Int -> [Bool]
lista_false 0 = []
lista_false n =  False : lista_false (n-1)

inc_binary :: [Bool] -> [Bool]
inc_binary xs = if last xs == False then (init xs) ++ [True] else (inc_binary(init xs)) ++ [False]

make_map :: [Char] -> [Bool] -> Map Char Bool
make_map [] _ = empty
make_map _ [] = empty
make_map xs ys = fromList (zip xs ys)

jest_tautologia :: Zdanie -> Bool
jest_tautologia zdanie = do
   let vars = unique $ quicksort $ wydobadz_zmienne zdanie
       vals = lista_false $ length vars
   tautologia' zdanie vars vals 0
   
tautologia' :: Zdanie -> [Char] -> [Bool] -> Integer -> Bool
tautologia' zdanie vars vals i = do
   let poprawnosc = sprawdz zdanie (make_map vars vals)
   if poprawnosc == False then False else if i == (2^(length vals)) - 1 then True 
      else tautologia' zdanie vars (inc_binary vals) (i+1)


przyklad = (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r')))
wartosci = fromList [('p', False), ('q', True), ('r', False)]