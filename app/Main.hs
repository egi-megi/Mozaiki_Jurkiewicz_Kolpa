{-# LANGUAGE FlexibleContexts #-}
import Prelude
import System.IO
import Data.Char(digitToInt)

-- wczytanie łamigłówki z pliku o podanej nazwie
readPuzzle :: String -> IO[String]
readPuzzle filename = do
  contents <- readFile filename -- odczytanie całej zawartości pliku
  let puzzle = read contents :: [String] -- utworzenie listy napisów
  return puzzle

-- wczytanie długości i szerokości tabeli
puzzleLength l = length l
puzzleWidth w = length $ head $ w

zero x y = 0 -- macierz samych 0 (będzie to macierz końcowa)
minusOne x y = -1 -- macierz samych -1 (będzie to macierz wejściowa)

-- wstawianie liczb do macierzy
set array (x,y) c =  (\x1 y1 -> if ((x==x1) && (y==y1)) then c else array x1 y1 )

-- wczytywanie pierwszego wiersza z pliku wejściowego *.txt
readLineToArray (c:xs) arInput x y | c/='.' = readLineToArray xs (set arInput (x,y) (digitToInt c)) (x+1) y | otherwise = readLineToArray xs arInput (x+1) y
readLineToArray [] arInput x y = arInput

-- wczytywanie wszystkich wierszy z pliku wejściowego *.txt
readAllLinesToArray (z:zs) arInput x y = readAllLinesToArray zs (readLineToArray z arInput x y) x (y+1)
readAllLinesToArray [] arInput x y = arInput

makeListOfElem w l n m = [(x,y) | x <- [n..w], y <- [m..l]] -- lista pozycji w macierzy do sprawdzenia

-- sprawdzenie czy suma elementów w macierzy końcowej jest równa bądź mniejsza niż cyfra w elemencie sprawdzanym
okCondition endArray inputArray (x,y) comparator w l |
  x<0 = True |
  y<0 = True |
  x>=w = True |
  y>=l = True |
  (inputArray x y) == -1 = True|
  otherwise = comparator ((endArray x y) +
                          (endArray (x+1) y) +
                          (endArray (x-1) y) +
                          (endArray x (y+1)) +
                          (endArray (x+1) (y+1)) +
                          (endArray (x-1) (y+1)) +
                          (endArray x (y-1)) +
                          (endArray (x+1) (y-1)) +
                          (endArray (x-1) (y-1)))  (inputArray x y)

makeListToSearch (x,y) = makeListOfElem (x+1) (y+1) (x-1) (y-1)

--czy warunki wokół elementu mogą by spełnione
mayBeGood endArray inputArray (x,y) comperator w l =  okCondition endArray inputArray (x,y) comperator  w l
                                                  && okCondition endArray inputArray ((x-1),y) comperator  w l
                                                  && okCondition endArray inputArray ((x-1),(y-1)) (==)  w l
                                                  && okCondition endArray inputArray ((x-1),(y+1)) comperator  w l
                                                  && okCondition endArray inputArray (x,(y-1)) comperator  w l
                                                  && okCondition endArray inputArray (x,(y+1)) comperator  w l
                                                  && okCondition endArray inputArray ((x+1),y) comperator  w l
                                                  && okCondition endArray inputArray ((x+1),(y-1)) comperator  w l
                                                  && okCondition endArray inputArray ((x+1),(y+1)) comperator  w l

-- czy wszystkie warunki sa spelnione
isGood endArray inputArray (x:xs) comparator  w l = (okCondition endArray inputArray x comparator  w l) && (isGood endArray inputArray xs comparator  w l)
isGood endArray inputArray [] comperator  w l = True

-- sprawdzenie czy w elemencie w górnym lewym rogu, czyli takim, do którego już w następnym kroku nie zajrzymy, spełniony jest warunek równości
equalityTestForNotReturningElem inputArray endArray ((x,y):restOfAllElements) w l |
  okCondition endArray inputArray ((x-1),(y-1)) (==) w l = makeSolution inputArray endArray restOfAllElements  w l
  | otherwise = Nothing

-- sprawdzanie kolejnych elementw z listy
makeSolution inputArray endArray ((x,y):restOfAllElements) w l  =
    let setOne = set endArray (x,y) 1 in
      if mayBeGood setOne inputArray (x,y) (<=)  w l then 
        let res = makeSolution inputArray setOne restOfAllElements  w l in
          case (res) of
              Nothing -> equalityTestForNotReturningElem inputArray endArray ((x,y):restOfAllElements) w l 
              Just a -> Just a
      else
        equalityTestForNotReturningElem  inputArray endArray ((x,y):restOfAllElements) w l
             
makeSolution inputArray endArray [] w l |
  isGood endArray inputArray (makeListOfElem (w-1) (l-1) 0 0) (==)  w l = Just endArray
  |otherwise = Nothing

-- wyświetlenie rozwiązania
-- dodano znaki dwch spacji pomiędzy wyświetlanymi cyframi, aby wyświetlany obrazek (macierz) był bardziej kwadratowy
printSolutionA endArray ((x,y):zs) n w | n < (w - 1) = do
                                                          putStr(show $ endArray y x)
                                                          putStr("  ")
                                                          printSolutionA endArray zs (n+1) w
                                       | otherwise = do
                                                        putStrLn(show $ endArray y x)
                                                        printSolutionA endArray zs 0 w

printSolutionA endArray [] n w   =   putStrLn( " ")                                                 
                                                    
printSolution Nothing lisOfAllElem n w = putStrLn(show $ "Unsolved ")
printSolution (Just endArray) lisOfAllElem n w = printSolutionA endArray lisOfAllElem n w

-- wyświetlenie macierzy wejściowej lub końcowej zawierającej wartości wierzchołków (jest to funkcja dodatkowa używana do debugowania)
printSolutionE endArray ((x,y):zs) n w | n < (w - 1) = do
                                                          putStr(show $ (x, y))
                                                          putStr(show $ ' ')
                                                          printSolutionE endArray zs (n+1) w
                                       | otherwise = do
                                                        putStrLn(show $ (x,y))
                                                        printSolutionE endArray zs 0 w
                                                        
-- funkcja dodatkowa do wypisywania czegokolwiek z macierzy -- służy do debugowania
printJust (Just a) x y = a x y
printJust Nothing x y = -1


main = do
  puzzle <- readPuzzle "puzzle.txt" -- wczytanie pliku
  let l = puzzleLength puzzle -- odczytanie liczby wierszy
  let w = puzzleWidth puzzle -- odczytanie liczby znakw w wierszu
  let listOfAllElem = makeListOfElem (w-1) (l-1) 0 0 -- wypisanie wszystkich wspłrzędnych w macierzy
  let inputArray = readAllLinesToArray puzzle minusOne 0 0 -- wczytanie znaków z pliku wejściowego do macierzy wejściowej
  let result = (makeSolution inputArray zero listOfAllElem w l) -- obliczenie macierzy końcowej
  printSolution result listOfAllElem 0 w -- wyświetlanie łamigłówki
