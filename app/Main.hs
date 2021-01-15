{-# LANGUAGE FlexibleContexts #-}
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
set array (x,y) c =  (\x1 y1 -> if ((x==x1) && (y==y1)) then c else array x1 y1 )

-- wczytaj długosć i szerokosć tabeli
puzzleLength p = length p
puzzleWidth pw = length $ head $ pw

-- wczytywanie pierwszego wiersza z pliku wejściowego *.txt
readLineToArray (c:xs) ar x y | c/='.' = readLineToArray xs (set ar (x,y) (digitToInt c)) (x+1) y | otherwise = readLineToArray xs ar (x+1) y
readLineToArray [] ar x y = ar

-- wczytywanie wszystkich wierszy z pliku wejściowego *.txt
readAllLinesToArray (z:zs) arInput y = readAllLinesToArray zs (readLineToArray z arInput 0 y) (y+1)
readAllLinesToArray [] arInput y = arInput

input = set zero (1,4) 7 -- macierz wejściowa

makeListOfElem w l n m = [(x,y) | x <- [n..w], y <- [m..l]] -- lista pozycji w macierzy do sprawdzenia

-- sprawdzenie czy suma elementów w macierzy końcowej jest równa bądź mniejsza niż cyfra w elemencie sprawdzanym
okCondition endArray inputArray (x,y) comparator = comparator ((endArray x y) + (endArray (x+1) y) + (endArray (x-1) y) + (endArray (x+1) (y+1)) + (endArray (x+1) (y-1)) + (endArray (x-1) (y+1)) + (endArray (x+1) (y-1)) + (endArray x (y+1)) + (endArray x (y-1)))  (inputArray x y) 
             
makeListToSearch (x,y) = makeListOfElem (x+1) (y+1) (x-1) (y-1)  

--czy warunki wokol elemntu moga byc spelnion
mayBeGood endArray inputArray (x,y) comperator =  okCondition endArray inputArray (x,y) comperator
                                                  && okCondition endArray inputArray ((x-1),y) comperator
                                                  && okCondition endArray inputArray ((x-1),(y-1)) comperator
                                                  && okCondition endArray inputArray ((x-1),(y+1)) comperator
                                                  && okCondition endArray inputArray (x,(y-1)) comperator
                                                  && okCondition endArray inputArray (x,(y+1)) comperator
                                                  && okCondition endArray inputArray ((x+1),y) comperator
                                                  && okCondition endArray inputArray ((x+1),(y-1)) comperator
                                                  && okCondition endArray inputArray ((x+1),(y+1)) comperator
                                          
-- wstawienie 0 lub 1 w określonym położeniu końcowej macierzy                                                
putOneOrZero endArray ((x,y):zs) n = set endArray (x,y) n

-- czy wszystkie warunki sa spelnione
isGood endArray inputArray listOfAllElements comparator = okCondition endArray inputArray (head listOfAllElements) comparator && isGood endArray inputArray (tail listOfAllElements) comparator
isGood endArray inputArray [] comperator = True

testZero inputArray endArray ((x,y):restOfAllElements) l w= makeSolution inputArray endArray restOfAllElements l w
  

makeSolution inputArray endArray ((x,y):restOfAllElements) w l  = 
   ( let with_one = (set endArray (x,y) 1) in
      if (mayBeGood with_one inputArray (x,y) (<=)) then 
        let res = makeSolution inputArray with_one restOfAllElements w l in 
          case (res) of
              Nothing -> (testZero inputArray endArray ((x,y):restOfAllElements) w l )
              Just a -> Just a
      else
        testZero  inputArray endArray ((x,y):restOfAllElements) w l)

makeSolution inputArray endArray [] w l | 
  isGood endArray inputArray (makeListOfElem (w-1) (l-1) 0 0) (==) = Just endArray 
  |otherwise = Nothing
         
 




--ok_cond array input_array x y comparator -- czy suma elementow kolo tego jest comparator wzgledem wrunkow poczatkowych

--may_be_good array input_array x y -- czy warunki wokol elemntu moga byc spelnione

--is_good array input_array l w  -- czy wszysteki warunki sa spelnione

-- jedziesz po tej liscie
-- na poczatku element  do sprawdzenia - ustawiasz jeden i patrzysz czy moze byc dobrze (<=)
-- jesli tak jedziesz dalej, jesli wtedy wrocisz bez rozwiazania, zmieniasz na zero jedziesz dalej
-- jesli jest pusta lista sprawdz wszytskie elementy czy sa dobrze (==) zwracasz rozwiazanie - jak nie nic
printJust (Just a) x y = a x y
printJust Nothing x y = -1

main = do
  puzzle <- readPuzzle "puzzle.txt"
  let l = puzzleLength puzzle 
  let w = puzzleWidth puzzle 
  let lisOfAllElem = makeListOfElem (w-1) (l-1) 0 0 
  let inputArray = readAllLinesToArray puzzle zero 0
  let res=(makeSolution inputArray zero lisOfAllElem w l)
  putStrLn(show $ printJust res 2 2)
  
  
  --putStrLn(show $ ((readAllLinesToArray puzzle zero 0 ) 6 9 ))
  --putStrLn(show $ ((readLineToArray (head puzzle) zero 0 0) 2 0 ))
  --putStrLn(show $ w)
  --putStrLn(show $ l) -- wyświetl liczbę wierszy łamigłówki
  --putStrLn(show $ (input 1 1) )
  --putStrLn(show $ (input 1 4))
  --putStrLn(show $ (input (-2) (-3)))