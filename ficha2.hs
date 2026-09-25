import Data.Char

--exercicio 2

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre char (h:t) | char == h = (1 + numOcorre char t)
                  | otherwise = (numOcorre char t)

positivos :: [Int] -> Bool
positivos [] = False
positivos [x] | x >= 0 = True
              | otherwise = False
positivos (h:t) | h >= 0 = positivos t
                | otherwise = False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h <= 0 = soPos t
            | otherwise = h : soPos t

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t

tresUlt :: [a] -> [a]
tresUlt (h:t) | length (h:t) <= 3 =(h:t)
              | otherwise = tresUlt t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((h1,h2):t) = h2 : segundos t

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((h1,h2):t) | x == h1 = True
                           | otherwise = nosPrimeiros x t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((h1,h2,h3):t) = (h1 + t1, h2 + t2, h3 + t3)
    where (t1,t2,t3) = sumTriplos t

--exercicio 3


soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = (h : soDigitos t)
                | otherwise = soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t
                 | otherwise = minusculas t

nums :: String -> [Int]
nums [] = []
nums (h:t) | (isDigit h) = (digitToInt h : nums t)
           | otherwise = nums t

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- [(2,3), (3,4), (5,3), (4,5)] -> 2x³ + 3x⁴ + 5x³ + 4x⁵

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((h1,h2):t) | n == h2 = 1 + conta n t
                    | otherwise = conta n t

grau :: Polinomio -> Int
grau [] = 0


deriv :: Polinomio -> Polinomio
deriv ((h1,h2):t) = ((h1 * (fromIntegral h2),h2 - 1): deriv t)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((h1,h2):t) | h1 == 0 = simp t
                 | otherwise = (h1,h2) : simp t

mult :: Monomio -> Polinomio -> Polinomio
