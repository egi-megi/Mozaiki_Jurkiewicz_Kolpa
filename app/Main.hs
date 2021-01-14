import Prelude
import System.IO
-- wczytaj łamigłówkę z pliku o podanej nazwie
readPuzzle :: String -> IO[String]
readPuzzle filename = do
  contents <- readFile filename -- odczytaj całą zawartość pliku
  let puzzle = read contents :: [String] -- utwórz listę napisów (zob. klasa typów Read)
  return puzzle 

zero x y = 0 -- macierz samych 0
set array x y c = (\x1 y1 -> if ((x==x1) && (y==y1)) then c else array x y )

-- wczytaj długosć i szerokosć tabeli
puzzleLength p = length p
puzzleWidth pw = length $ head $ pw



input = set zero 1 4 7 -- macierz wejściowa

main = do 
  puzzle <- readPuzzle "puzzle.txt" 
  let l = puzzleLength puzzle
  let w = puzzleWidth puzzle
  putStrLn(show $ w)
  putStrLn(show $ l) -- wyświetl liczbę wierszy łamigłówki
  putStrLn(show $ (input 1 1) ) 
  putStrLn(show $ (input 1 4)) 