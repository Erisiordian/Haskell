import Data.Char
import Data.List
--import Data.List
--zad 1 za vježbu
poly :: Int->Int
poly x = 2*x^2 + 4*x + 7

--zad 2 za vježbu
sumn :: Int -> Int
sumn 0 = 0
sumn 1 = 1
sumn n = n + sumn (n-1) 

--zad 3 za vježbu
sumsq :: Int -> Int
sumsq 0 = 0
sumsq 1 = 1
sumsq n = n*n + sumsq (n-1)

--zad 4 za vježbu
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

--zad 5 za vježbu
sumListEven :: [Int] -> Int
sumListEven [] = 0
sumListEven (x:[]) = x
sumListEven (x:_:xs) = x + sumListEven xs

--zad 6 za vježbu
vecSum (x:xs) (y:ys) = x+y : vecSum xs ys
vecSum _ _ = []

--zad 7 za vježbu
sumBetween :: Int-> Int-> Int
sumBetween a b = sum [a..b]

--zad 8 za vježbu
is_odd :: Int-> Bool
is_odd 0 = False
is_odd 1 = True
is_odd n = is_odd(n-2)

--zad 9 za vježbu
nospace s = filter (\c -> c /= ' ') s

--zad 10 za vježbu
--toPairs :: [a] -> [b] -> [(a,b)]
toPairs (x:xs) (y:ys) = (x,y) : toPairs xs ys
toPairs _ _ = []

--zad 11 za vježbu
horner _ [] = 0
horner x (y:ys) = y+x*horner x ys

--zad 1 demosi1
dpoly x = 2*x +1

--zad 2 demosi1
prodn 0 = 1
prodn n = n * prodn(n-1)

--zad 3 demosi1
lenList [] = 0
lenList (x:xs) = 1 + lenList xs

--zad 4 demosi1
sumListOdd [] = 0
sumListOdd [_] = 0
sumListOdd (_:y:ys) = y + sumListOdd ys

--zad 5 demosi1
dotProd [] [] = 0
dotProd (x:xs) (y:ys) = x*y + dotProd xs ys

--zad 6 demosi1
countA [] = 0
countA ('a' : xs) = 1 + countA xs
countA ('A' : xs) = 1 + countA xs
countA (_ : xs) = countA xs

--zad 7 demosi1
digitSum 0 = 0
digitSum n = ( n `mod` 10) + digitSum (n `div` 10)

--zad 8 demosi1
dig2num' [] acc = acc
dig2num' (x:xs) acc = dig2num' xs (10*acc + x)

dig2num ls = dig2num' ls 0

--zad 9 demosi1
is_even 0 = True
is_even 1 = False
is_even n = is_even (n-2)

--zad 10 demosi1
rep _ 0 = []
rep n i = n : rep n (i-1)

--zad 11 demosi1
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

--demosi3
--funkcija map primjer
--primjer: 2*x+1
mList l = map(\x -> 2*x +1) l

--demosi3 zad 4
func ' ' = ' '
func c = chr $ (ord c - ord 'a' + 3) `mod` 26 + ord 'a'

ceasar s = map func s

--demosi3 zad 15
derive p = map (\(a,k) -> k*a) (zip (tail p) [1..n])
  where n = length p - 1
  
--demosi3 zad 12
--vecSum preko funkcije map

--demosi3 zad 1
{- 
transpose je gotova funkcija u Data.List inače izgleda ovako:-}
{-transpose ([]:_) = []
transpose m = (map head m) : (transpose (map tail m))-}
--s Data.List potrebno je samo ovo
swap m = transpose (reverse m)

--filter funkcija filitrira elemente koji zadovljavaju uvjet

--demosi3 zad 8
noSpaces s = filter (\x -> x /=' ') s

--demosi3 zad 1
--sumPerf
divisors n = filter(\k-> n `mod` k == 0) [1..n]
perfect n = sum (divisors n) == 2*n
sumPerf n = sum $ filter perfect [1..n] 

--demosi3 zad 2
--pitagorina trojka


--demosi3 foldl i foldr
--foldl gomila zagrade na lijevo
--foldr gomila zagrade na desno
--demosi3 zad 16
listtoInt l = foldl (\acc a-> 10*acc+a) 0 l

--demosi3 zad 3
minmax (h:xs) =  foldl (\(a,b) x -> (min x a, max x b)) (h, h) xs

--demosi3 zad 3
--bez fodl ili foldr
{-
norm_1 [] = 0
norm_1 (x:xs) = abs x + norm_1 xs 
-}
--sa foldl i foldr ili map
norm_1 xs = sum $ map abs xs

--demosi3 zad 11
norm v = sqrt $ foldr (\a acc -> acc + a^2) 0 v

--demosi3 zad 2
maxnorm (v:vs) = foldl (\acc x -> if norm x > norm acc then x else acc) v vs

--demosi3 zad 1
polinom p x = foldl (\acc a -> x*acc + a) 0 p

