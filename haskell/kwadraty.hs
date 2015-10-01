-- Pawel Jankowski
-- Zad nr. 1 "Kwadraty"

-- Celem programu jest wypisanie kwadratow ktore posiadaja w jednym z ich
-- rogow cyfre, rowna ilosci liczb ktore ten kwadrat obejmuje. Boki
-- kwadratow i rogi nie moga się stykac/pokrywac.
-- 
-- W projekcie staralem sie nazwy funkcji zlozone z wielu slow
-- zapisywac za pomoca CamelCase.
-- 
-- Ogolie LT oznacza Left Top
-- Ogolie RT oznacza Right Top
-- Ogolie LB oznacza Left Bottom
-- Ogolie RB oznacza Right Bottom
-- 
-- Dolaczone sa testy studenta, na moim
-- komputerze po uruchomieniu Checkera czas wynosi zawsze 0.00s 

------------------------------------------------
-- Algorytm:
-- 
-- Algorytm polega na tym ze generuje kwadraty tak jakby ich cyfra byla
-- wpisana w lewy-gorny, lewy-dolny, prawy-gorny, prawy-dolny rog,
-- oczywiscie sprawdzajac kolizje z cyframi wpisanymi w plansze i ile
-- cyfr ich obwod obejmuje. Dalej generuje rozwiazanie biarac pod uwage
-- kolizje z innymi kwadratami. 
-------------------------------------------------


import Puzzle
import Checker
import Control.Monad


type Tuple = (Int, Int, Int)
type Board = [(Int, Int, Int)]
type Size = (Int, Int)



-- Fukcja ktora przyjmuje wierzcholek kwadratu w LT (o poczatku w lewym gornym rogu)
-- i plansze, ustala ile jest liczb w srodku tego kwadratu. 
-- d to dlugosc boku kwadratu 
digitsInSquare :: Tuple -> Board -> Int  
digitsInSquare (_, _, _) [] = 0
digitsInSquare (row, col, d) ((a, b, _):xs) 
  | (between rowLT rowRB a) && (between colLT colRB b) = 1 + digitsInSquare (row, col, d) xs
  | otherwise = digitsInSquare (row, col, d) xs 
    where rowLT = row + 1
          colLT = col + 1
          rowRB = row + d - 1
          colRB = col + d - 1


-- Funkcja zwracajaca liste mozliwych kwadratow, gdzie cyfra znajduje sie w ich lewym
-- gornym rogu. Funkcja sprawdza ile kwadrat zawiera cyfr i czy przypadkiem 
-- nie ma cyfry na boku wygenerowanego kwadratu. 
-- Funkcja przyjmuje wymiary planszy, dany punkt i plansze. Fukcja korzysta z list comprehension.
-- rowNext, rowBefore, colNext, colBefore to wiersze i kolumny inkrementowane/dekrementowane
genSquaresFromLT :: Size -> Tuple ->  Board -> [Tuple]
genSquaresFromLT (m,n) (row, col, val) board = [(row, col, d) | 
    rowNext <- [row+1..m], 
    let d = rowNext - row,
    let colNext = col + d in colNext <= n,
    val == (digitsInSquare (row, col, d) board),
    isDigitInDistrict (row, col, d) (row, col, val) board == False]    


-- Funkcja zwracajaca liste mozliwych kwadratow, gdzie cyfra znajduje sie w ich prawym
-- gornym rogu. Funkcja sprawdza ile kwadrat zawiera cyfr i czy przypadkiem 
-- nie ma cyfry na boku wygenerowanego kwadratu.
-- Funkcja przyjmuje wymiary planszy, dany punkt i plansze. Fukcja korzysta z list comprehension.
-- rowNext, rowBefore, colNext, colBefore to wiersze i kolumny inkrementowane/dekrementowane
genSquaresFromRT :: Size -> Tuple -> Board -> [Tuple]
genSquaresFromRT (m, _) (row, col, val) board = [(row, col - d, d) |
    rowNext <- [row+1..m],
    let d = rowNext - row,
    let colBefore = col - d in colBefore >= 1,
    val == (digitsInSquare (row, col - d, d) board),
    isDigitInDistrict (row, col - d, d) (row, col, val) board == False]    


-- Funkcja zwracajaca liste mozliwych kwadratow, gdzie cyfra znajduje sie w ich lewym
-- dolnym rogu. Funkcja sprawdza ile kwadrat zawiera cyfr i czy przypadkiem 
-- nie ma cyfry na boku wygenerowanego kwadratu.
-- Funkcja przyjmuje wymiary planszy, dany punkt i plansze. Fukcja korzysta z list comprehension.
-- rowNext, rowBefore, colNext, colBefore to wiersze i kolumny inkrementowane/dekrementowane
genSquaresFromLB :: Size -> Tuple -> Board -> [Tuple] 
genSquaresFromLB (_, n) (row, col, val) board = [(row - d, col, d) |
    colNext <- [col+1..n],
    let d = colNext - col,
    let rowBefore = row - d in rowBefore >= 1,
    val == (digitsInSquare (row - d, col, d) board),
    isDigitInDistrict (row - d, col, d) (row, col, val) board == False]    


-- Funkcja zwracajaca liste mozliwych kwadratow, gdzie cyfra znajduje sie w ich prawym
-- dolnym rogu. Funkcja sprawdza ile kwadrat zawiera cyfr i czy przypadkiem 
-- nie ma cyfry na boku wygenerowanego kwadratu.
-- Funkcja przyjmuje wymiary planszy, dany punkt i plansze. Fukcja korzysta z list comprehension.
-- rowNext, rowBefore, colNext, colBefore to wiersze i kolumny inkrementowane/dekrementowane
genSquaresFromRB :: Size -> Tuple -> Board -> [Tuple]
genSquaresFromRB (_, _) (row, col, val) board = [(row - d, col - d, d) |
    colBefore <- [col-1,col-2..1],
    let d = col - colBefore,
    let rowBefore = row - d in rowBefore >= 1,
    val == (digitsInSquare (row - d, col - d, d) board),
    isDigitInDistrict (row - d, col - d, d) (row, col, val) board == False]    


-- Funkcja kotra generuje wszystkie mozliwe kwadraty z danego punktu 
-- (kwadraty, gdy cyfra jest w LT, LB, RT, RB) 
-- Funkcja przyjmuje wymiary planszy dany punkt na planszy i sama plansze.
genSquarePossiblietesFromPoint :: Size -> Tuple -> Board -> [Tuple]
genSquarePossiblietesFromPoint (m, n) (row, col, val) board = (genSquaresFromLT x y z) 
    ++ (genSquaresFromRT x y z) 
    ++ (genSquaresFromLB x y z) 
    ++ (genSquaresFromRB x y z)
  where x = (m, n) 
        y = (row, col, val)
        z = board


-- Funkcja kotra generuje wszystkie mozliwe kwadraty ze wszystkich punktow na planszy 
-- zwraca liste list z kwadratami.  Pryjmuje wymiary planszy i dwa razy plansze.
genListofAllSquaresInBoard :: Size -> Board -> Board ->  [[Tuple]]
genListofAllSquaresInBoard (_ , _) [] _ = []
genListofAllSquaresInBoard (m, n) (x:xs) board = [(genSquarePossiblietesFromPoint (m,n) x board)] 
   ++ (genListofAllSquaresInBoard (m,n) xs board) 


-- Glowna funkcja sluzaca wygenerowaniu listy poprawnych rozwiazan, 
-- wybiera krotke sprawdza czy nie ma kolizjii z innymi juz wybranymi, i jesli wszystko 
-- jest w porzadku zwraca rozwiazanie (takie nawroty z Prologa).
prune :: [Tuple] -> [[Tuple]] -> [[Tuple]]
prune acc [] = [racc] where racc = reverse acc
prune acc (ks:kss) = do
    k <- ks
    guard (all (isNotCollision k) [acc])
    prune (k:acc) kss


-- "Zanegowana" funkcja isCollision
isNotCollision :: Tuple -> [Tuple] -> Bool
isNotCollision x y = not (isCollision x y)


-- Funkcjia sprawdzajaca czy dana liczba jest pomiedzy danymi dwoma wartosciami.
between :: Int -> Int -> Int -> Bool
between beg end x 
  | x >= beg && x <= end = True
  | otherwise = False


-- Funkcja pozwalajaca stwierdzic czy jest cyfra z planszy na ktoryms z bokow w kwadracie,
-- oczywiscie procz tego z ktorego rozpoczynamy, dlatego go filtrujemy
-- i dlatego potrzebny nam ten drugi parametr. Funkcja przyjmuje kwadrat, punkt z ktorego zaczynamy i plansze.
-- d to dlugosc boku kwadratu  
isDigitInDistrict :: Tuple -> Tuple -> Board -> Bool
isDigitInDistrict (rowLT, colLT, d) (row, col, val) board = isDigitInDistrictHelp tup (filter  (/= (row, col, val)) board)  
  where tup = (rowLT, colLT, d)


-- Funkcja pozwalajaca stwierdzic czy jest cyfra na ktoryms z bokow w kwadracie.
-- Funkcja przyjmuje kwadrat i plansze.
-- d to dlugosc boku kwadratu  
-- zmienna z koncowka B odnosi sie do elementu z planszy, duze D oznacza prawy dolny rog
isDigitInDistrictHelp :: Tuple -> Board -> Bool
isDigitInDistrictHelp (_, _, _) [] = False
isDigitInDistrictHelp (row, col, d) ((rowB, colB, _):xs) = 
    if 
       ( (between row rowD rowB) && (col == colB || colD == colB) ) ||
          ( (between col colD colB) && (row == rowB || rowD == rowB)) then  True
   else isDigitInDistrictHelp (row, col, d) xs
     where
         rowD = row + d
         colD = col + d


-- Funkcja zwraca true jesli pewne boki sie pokrywaja, stykaja.
-- zmienna z koncowka A oznacza kwadrat z ktorym sprawdzamy kolizje
-- D na koncu oznacza prawy dolny rog
isCollision :: Tuple -> [Tuple] -> Bool 
isCollision (_, _, _) [] = False
isCollision (row, col, d) ((rowA, colA, dA):xs) = 
    if 
       ( ((row == rowA || rowD == rowA || row == rowAD || rowD == rowAD) && 
      ((between colA colAD col) || (between colA colAD colD) || (between col colD colA) || (between col colD colAD) )) ||
        ((col == colA || colD == colA || col == colAD || colD == colAD) && 
          ((between rowA rowAD row) || (between rowA rowAD rowD) || (between row rowD rowA) || (between row rowD rowAD))) ) 
          then True
            else isCollision (row, col, d) xs
              where 
                  rowAD = rowA + dA
                  colAD = colA + dA
                  rowD = row + d
                  colD = col + d


-- Generujemy dla kazdego punktu planszy liste jego mozliwych kwadratow i uzywamy monady do "filtracji" tych ktore spelniaja warunki zadania.
solve :: Solver
solve m n board = prune [] (genListofAllSquaresInBoard (m, n) board board)


tests :: [Test]
tests = [ SimpleTest (Puzzle 7 7 [(3,3,0), (3,5,0), (4,4,1), (5,1,0), (6,6,3)]) 

    [(1,1,2), (1,5,2), (4,4,3), (5,1,2), (2,2,4)],



          SimpleTest (Puzzle 14 14 [(1,1,6),(1,12,0),(2,2,0),(2,4,1),(2,8,0),(3,5,1),(4,2,0),(4,12,0),
            (6,7,1),(7,14,1),(9,12,0),(10,3,2),(11,9,0),(11,12,1),(12,4,0), (12,5,0),(12,9,0),(12,11,0)]) 

                [(1,1,8),(1,12,2),(2,2,1),(2,4,3),(2,8,3),(3,5,5),(4,2,1),(4,12,2), (6,2,5),(7,11,3),(8,12,1),
                (10,3,3),(10,8,1),(11,10,2),(12,2,2), (12,5,2),(12,8,1),(12,11,2)],



          SimpleTest (Puzzle 20 28 [(3,3,4), (3,20,1), (3,26,0), (4,18,1), (7,22,1), (9,22,0), (11,18,2), (16,13,1), (18,18,0), (20,15,1)]) 

          [(3, 3, 16), (3, 20, 5), (3, 26, 1), (1, 18, 3), (1, 22, 6), (9, 22, 6), (5, 18, 6), (9, 13, 7), (18, 18, 2), (6, 1, 14)],
--sdklfjksldfjklsdjfklsdfkls
          SimpleTest (Puzzle 20 20 [(1,1,4),(1,9,1),(1,19,1),(2,2,4),(2,13,0),(2,18,0),(3,3,0),(3,6,1),(6,3,0),
            (7,11,0),(7,15,2),(8,17,0),(9,3,0),(9,17,0),(12,1,1),(12,14,2),(13,2,3),
                (13,17,0),(14,8,1),(15,3,0),(15,4,0),(15,6,1),(15,12,1),(15,17,0),(16,9,1),
                    (18,11,0),(18,16,0),(19,1,0),(19,5,0),(19,18,0)])

                    [(1,1,6),(1,9,5),(1,17,2),(2,2,8),(2,11,2),(2,16,2),(3,3,1),(3,6,6),
                         (5,3,1),(5,11,2),(7,15,3),(6,17,2),(8,3,1),(9,17,2),(12,1,2),(12,14,5),
                              (13,2,5),(13,17,1),(14,8,3),(15,1,2),(14,4,1),(12,6,3),(11,12,4), 
                                  (15,17,1),(16,9,3),(18,11,2),(18,14,2),(19,1,1),(19,4,1),(18,17,1)],


          CountTest (Puzzle 5 5 [(1,1,1),(2,2,0)]) 6,
          CountTest (Puzzle 9 5 [(2,2,1),(3,3,1),(5,5,1),(6,4,0)]) 6,
          CountTest (Puzzle 3 10 [(1,1,0),(1,3,0),(2,5,0),(1,7,0),(2,8,0)]) 0,
          CountTest (Puzzle 9 12 [(3,10,1), (4,8,1), (7,4,0), (7,8,0), (8,3,3)]) 11]


main :: IO ()
main = checkerMain solve tests
