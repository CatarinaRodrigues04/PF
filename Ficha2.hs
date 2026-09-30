import Data.Char
import Data.List

-- Exercício 1.
-- a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
-- funA [2,3,5,1] -> 4 + funA[3,5,1] -> 13 + funA[5,1] -> 38 + funA[1] -> 39

-- b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h : (funB t)
                               else (funB t)
-- funB [8,5,12] -> [8, funB[5,12]] -> [8, funB[12]] -> [8, 12]

-- c)
funC :: [a] -> [a]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []
-- funC [1,2,3,4,5] -> funC[3,4,5] -> funC[5] -> [5]

-- d)
funD :: [a] -> [a]
funD l = g [] l

g :: [a] -> [a] -> [a]
g acc [] = acc
g acc (h:t) = g (h:acc) t
-- funD "otrec" -> g [] "otrec" -> g "o" "trec" -> g "to" "rec" -> g "rto" "ec" -> g "erto" "c" -> g "certo" [] -> "certo"

-- Exercício 2.
-- a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h*2 : dobros t)

-- b)
numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) | x == h = 1 + numOcorre x t
                  | otherwise = numOcorre x t

-- c)
positivos :: [Int] -> Bool
positivos [] = False
positivos [x]   | x >= 0 = True
                | otherwise = False
positivos (h:t) | h >= 0 = positivos t
                | otherwise = False

-- d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h >= 0 = (h : soPos t)
            | otherwise = soPos t

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t

-- f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length (h:t) <= 3 = (h:t)
              | otherwise = tresUlt t

-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((h1,h2):t) = h2 : segundos t

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((h1,h2):t) | x == h1 = True
                           | otherwise = nosPrimeiros x t

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((h1,h2,h3):t) = (h1 + t1, h2 + t2, h3 + t3)
                                where (t1,t2,t3) = sumTriplos t

-- Exercício 3.
-- a)
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = (h : soDigitos t)
                | otherwise = soDigitos t

-- b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t
                 | otherwise = minusculas t

-- c)
nums :: String -> [Int]
nums [] = []
nums (h:t) | isDigit h = (digitToInt h : nums t)
           | otherwise = nums t

-- Exercício 4.
type Polinomio = [Monomio]
type Monomio = (Float,Int)
-- [(2,3), (3,4), (5,3), (4,5)] -> 2x^3+3x^4+5x^3+4x^5

-- a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((h1,h2):t) | n == h2 = 1 + conta n t
                    | otherwise = conta n t

-- b)
grau :: Polinomio -> Int
grau [] = 0
grau ((h1,h2):t) | h2 >= t2 = h2
                 | otherwise = t2
                    where t2 = grau t

-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((h1,h2):t) | n == h2 = (h1,h2) : selgrau n t
                      | otherwise = selgrau n t

-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((h1,h2):t) | h2 > 0 = (h1 * fromIntegral h2, h2 - 1) : deriv t
                  | otherwise = deriv t

-- e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((h1,h2):t) = h1 * x^h2 + calcula x t

-- f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((h1,h2):t) | h1 == 0 = simp t
                 | otherwise = ((h1,h2): simp t)

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c,e) ((h1,h2):t) | e == h2 = (c + h1, h2) : mult (c,e) t
                       | otherwise = mult (c,e) t

-- h)
adicionaMonomio :: Monomio -> Polinomio -> Polinomio
adicionaMonomio (c,e) [] = [(c,e)]
adicionaMonomio (c,e) ((h1,h2):t) | e == h2 = (c + h1, h2) : t
                                  | otherwise = (h1,h2) : adicionaMonomio (c,e) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = adicionaMonomio h (normaliza t)

-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p2 = soma (mult h p2) (produto t p2)

-- k)
insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (c,e) ((h1,h2):t) | e <= h2 = (c,e) : (h1,h2) :t
                         | otherwise = insere (c,e) t

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insere h (ordena t)

-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)