import Prelude
import System.IO
import Data.Char(digitToInt)

-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO[String]
readPuzzle filename = do
  contents <- readFile filename -- odczytaj całą zawartość pliku
  let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
  return puzzle 

zero x y = 0 -- macierz samych 0

-- wstawianie liczb do macierzy
set array x y c =  (\x1 y1 -> if ((x==x1) && (y==y1)) then c else array x1 y1 )

-- wczytaj długosć i szerokosć tabeli
puzzleLength p = length p
puzzleWidth pw = length $ head $ pw

-- wczytywanie pierwszego wiersza z pliku wejściowego *.txt
readLineToArray (c:xs) ar x y | c/='.' = readLineToArray xs (set ar x y (digitToInt c)) (x+1) y | otherwise = readLineToArray xs ar (x+1) y  
readLineToArray [] ar x y = ar

-- wczytywanie wszystkich wierszy z pliku wejściowego *.txt
readAllLinesToArray (z:zs) ar x y = readAllLinesToArray zs (readLineToArray z ar x y) x (y+1)
readAllLinesToArray [] ar x y = ar

input = set (set zero 1 4 7) 2 3 8 -- macierz wejściowa

main = do 
  puzzle <- readPuzzle "puzzle.txt" 
  let l = puzzleLength puzzle
  let w = puzzleWidth puzzle
  putStrLn(show $ ((readAllLinesToArray puzzle zero 0 0) 6 9 ))
  putStrLn(show $ ((readLineToArray (head puzzle) zero 0 0) 2 0 ))
  putStrLn(show $ w)
  putStrLn(show $ l) -- wyświetl liczbę wierszy łamigłówki
  putStrLn(show $ (input 1 1) ) 
  putStrLn(show $ (input 1 4))
  putStrLn(show $ (input (-2) (-3))) 