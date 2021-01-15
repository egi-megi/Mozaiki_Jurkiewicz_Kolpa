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

zero x y = 0 -- macierz samych 0
minus_one x y = -1 -- macierz samych -1

-- wstawianie liczb do macierzy
set array (x,y) c =  (\x1 y1 -> if ((x==x1) && (y==y1)) then c else array x1 y1 )

-- wczytanie długości i szerokości tabeli
puzzleLength l = length l
puzzleWidth w = length $ head $ w

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
  y>=w = True |
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

--czy warunki wokol elemntu moga byc spelnion
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

-- łamigłówki
testZero inputArray endArray ((x,y):restOfAllElements) w l |
  okCondition endArray inputArray ((x-1),(y-1)) (==)  w l= makeSolution inputArray endArray restOfAllElements  w l
  | otherwise = Nothing

-- końcowe wyliczenie 0 i 1 w łamigłówce
makeSolution inputArray endArray ((x,y):restOfAllElements)  w l  =
   ( let with_one = (set endArray (x,y) 1) in
      if (mayBeGood with_one inputArray (x,y) (<=)  w l) then
        let res = makeSolution inputArray with_one restOfAllElements  w l in
          case (res) of
              Nothing -> (testZero inputArray endArray ((x,y):restOfAllElements) w l )
              Just a -> Just a
      else
        testZero  inputArray endArray ((x,y):restOfAllElements) w l)
makeSolution inputArray endArray [] w l |
  isGood endArray inputArray (makeListOfElem (w-1) (l-1) 0 0) (==)  w l = Just endArray
  |otherwise = Nothing

-- wyświetlenie rozwiązania
printSolutionA endArray ((x,y):zs) n w | n < (w - 1) = do
                                                          putStr(show $ endArray y x)
                                                          putStr(show $ ' ')
                                                          printSolutionA endArray zs (n+1) w
                                       | otherwise = do
                                                        putStrLn(show $ endArray y x)
                                                        printSolutionA endArray zs 0 w
printSolution Nothing lisOfAllElem n w = putStrLn(show $ "Unsolved ")
printSolution (Just endArray) lisOfAllElem n w = printSolutionA endArray lisOfAllElem n w

-- wyświetlenie macierzy wejściowej lub końcowej (jest to funkcja dodatkowa używana do debugowania)
-- dodano znak ' ' pomiędzy cwyświetlanymi cyframi, aby wyświetlany obrazek (macierz) był bardziej kwadratowy 
printSolutionE endArray ((x,y):zs) n w | n < (w - 1) = do
                                                          putStr(show $ (x, y))
                                                          putStr(show $ ' ')
                                                          printSolutionE endArray zs (n+1) w
                                       | otherwise = do
                                                        putStrLn(show $ (x,y))
     
                                                        printSolutionE endArray zs 0 w
                                                        
-- funkcja ododatkowa do wypisywania czegokolwiek z macierzy -- służy do debugowania
printJust (Just a) x y = a x y
printJust Nothing x y = -1

--ok_cond array input_array x y comparator -- czy suma elementow kolo tego jest comparator wzgledem wrunkow poczatkowych
--may_be_good array input_array x y -- czy warunki wokol elemntu moga byc spelnione
--is_good array input_array l w  -- czy wszysteki warunki sa spelnione

-- jedziesz po tej liscie
-- na poczatku element  do sprawdzenia - ustawiasz jeden i patrzysz czy moze byc dobrze (<=)
-- jesli tak jedziesz dalej, jesli wtedy wrocisz bez rozwiazania, zmieniasz na zero jedziesz dalej
-- jesli jest pusta lista sprawdz wszytskie elementy czy sa dobrze (==) zwracasz rozwiazanie - jak nie nic


main = do
  puzzle <- readPuzzle "puzzle.txt" -- wczytanie pliku
  let l = puzzleLength puzzle -- odczytanie liczby wierszy
  let w = puzzleWidth puzzle -- odczytanie liczby znakw w wierszu
  let listOfAllElem = makeListOfElem (w-1) (l-1) 0 0 -- wypisanie wszystkich wspłrzędnych w macierzy
  let inputArray = readAllLinesToArray puzzle minus_one 0 0 -- wczytanie znaków z pliku wejściowego do macierzy wejściowej
  let result = (makeSolution inputArray zero listOfAllElem w l) -- oblliczenie macierzy końcowej
  printSolution result listOfAllElem 0 w -- wyświetlanie łamigłówki


  -- putStrLn(show $ w) -- wyświetlanie szerokości łamigłówki
  -- putStrLn(show $ l) -- wyświetlanie szerokości łamigłówki
  
  
    --putStrLn(show $ listOfAllElem) 
    --printSolutionA inputArray listOfAllElem 0 w
    
    --putStrLn(show $ printJust res 2 2)
    --putStrLn(show $ printJust (makeSolution (set ( set (set (set (set (set zero (3,3) 1) (3,2) 1) (2,3) 1) (2,2) 1) (3,1) 1 ) (2,1) 1) (set zero (3,2) 1) [(3,3)]  4 4) 3 3)
    --putStrLn(show $ isGood (set zero (3,2) 1) (set ( set (set (set (set (set zero (3,3) 1) (3,2) 1) (2,3) 1) (2,2) 1) (3,1) 1 ) (2,1) 1)  (makeListOfElem (3) (3) 0 0) (==)  4 4)
    --putStrLn(show $ okCondition (set zero (3,2) 1) (set ( set (set (set (set (set zero (3,3) 1) (3,2) 1) (2,3) 1) (2,2) 1) (3,1) 1 ) (2,1) 1)  (2,3) (==)  4 4)
  
    -- putStrLn(show $ isGood (set zero (3,3) 1) (set (set (set (set zero (3,3) 1) (3,2) 1) (2,3) 1) (2,2) 1)  (makeListOfElem (3) (3) 0 0) (==)  w l)
    -- putStrLn(show $ isGood (set zero (3,3) 1) (set (set zero (3,3) 1) (3,2) 1)  [(3,2),(3,3)] (==)  w l)
    -- putStrLn(show $ okCondition (set zero (3,3) 1) (set zero (3,3) 1) (-1,-1) (==)  w l)
    --putStrLn(show $ ((readAllLinesToArray puzzle zero 0 ) 6 9 ))
    --putStrLn(show $ ((readLineToArray (head puzzle) zero 0 0) 2 0 ))
