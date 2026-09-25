-- questão 1
_enumFromTo :: Int -> Int -> [Int]
-- _emumFromTo 1 5 -> [1,2,3,4,5]
_enumFromTo x y | x < y = (x : enumFromTo (x+1) y)
                | x == y = [x]
                | otherwise = []

-- quetsão 2
_enumFromThenTo :: Int -> Int -> Int -> [Int]
-- _enumFromThenTo 1 3 10 -> [1,3,5,7,9]
_enumFromThenTo x y z | x < z = (x : enumFromThenTo (x+y) y z)
                      | x == z = [x]
                      | otherwise = []

-- questão 3
plusplus :: [a] -> [a] -> [a]
-- plusplus [1,2,3] [10,20,30] = [1,2,3,10,20,30]
plusplus [] l = l
plusplus (h:t) [] = h : plusplus [] t
plusplus (h:t) l = h : plusplus t l

-- questão 4
exclExcl :: [a] -> Int -> a
-- exclExcl [10,20,30] 1 = 20
exclExcl (h:t) 0 = h
exclExcl (h:t) l = exclExcl t l

-- questão 5
_reverse :: [a] -> [a]
-- _reverse [10,20,30] -> [30,20,10]
_reverse [] = []
_reverse (h:t) = _reverse t ++ [h]

-- questão 6
_take :: Int -> [a] -> [a]
-- _take 2 [10,20,30] -> [10,20]
_take _ [] = []
_take 0 _ = []
_take n (h:t) = h : _take (n-1) t

-- questão 7
_drop :: Int -> [a] -> [a]
-- _drop 2 [10,20,30] -> [30]
_drop n [] = []
_drop n (h:t) | n > 0 = _drop (n-1) t
              | otherwise = h : _drop 0 t

-- questão 8
_zip :: [a] -> [b] -> [(a,b)]
-- _zip [1,2,3] [10,20,30,40] -> [(1,10),(2,20),(3,30)]
_zip [] _ = []
_zip (x:xs) (y:ys) = (x,y) : _zip xs ys

-- questão 9
_replicate :: Int -> a -> [a]
-- _replicate 3 10 -> [10,10,10]
_replicate 0 _ = []
_replicate n x = x : _replicate (n-1) x

-- quastão 10
_intersperse :: a -> [a] -> [a]
-- _intersperse 1 [10,20,30] -> [10,1,20,1,30]
_intersperse x [] = []
_intersperse x [a] = [a]
_intersperse x (h:t) = h : x : _intersperse x t

-- questão 11
_group :: Eq a => [a] -> [[a]]
-- _group [1,2,2,3,4,4,4,5,4] -> [[1],[2,2],[3],[4,4,4],[5],[4]]
_group [] = []
_group (h:t) = (h : takeWhile (==h) t) : _group (dropWhile (==h) t)

-- questão 12
_concat :: [[a]] -> [a]
-- _concat [[1],[2,2],[3],[4,4,4],[5],[4]] -> [1,2,2,3,4,4,4,5,4]
_concat [] = []
_concat (h:t) = h ++ concat t

-- questão 13
_inits :: [a] -> [[a]]
-- _inits [11,21,13] -> [[],[11],[11,21],[11,21,13]]
_inits [] = [[]]
_inits l = _inits (init l) ++ [l]

-- questão 14
_tails :: [a] -> [[a]]
-- _tails [1,2,3] -> [[1,2,3],[2,3],[3],[]]
_tails [] = [[]]
_tails (h:t) = (h:t) : _tails t

-- questão 15
heads :: [[a]] -> [a]
-- heads [[2,3,4],[1,7],[],[8,5,3]] -> [2,1,8]
heads [] = []
heads (h:t) | null h = heads t
            | otherwise = head h : heads t

-- questão 16
total :: [[a]] -> Int
-- total [[2,3,4],[1,7],[],[8,5,3]]
total [] = 0
total (h:t) = length h + total t

-- questão 17
fun :: [(a,b,c)] -> [(a,c)]
-- fun [("rui",3,2),("maria",5,2),("ana",43,7)] -> [("rui",2),("maria",2),("ana",7)]
fun [] = []
fun ((h1,h2,h3):t) = (h1,h3) : fun t

-- questão 18
cola :: [(String,b,c)] -> String
-- cola [("rui",3,2),("maria",5,2),("ana",43,7)] -> "ruimariaana"
cola [] = ""
cola ((h1,h2,h3):t) = h1 ++ cola t

-- questõa 19
idade :: Int -> Int -> [(String,Int)] -> [String]
-- idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] -> ["rui","ana"]
idade _ _ [] = []
idade x y ((h1,h2): t) | x - h2 < y = idade x y t
                       | otherwise = h1 : idade x y t

-- questão 20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom _ 1 = [1]
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [n^(m-1)]

-- questão 21
isPrime :: Int -> Bool
isPrime n | n <= 2 = True
          | otherwise = not (hasDivisors n 2)

hasDivisors :: Int -> Int -> Bool
hasDivisors n m | m * m > n = False
                | mod n m == 0 = True
                | otherwise = hasDivisors n (m+1)

-- questão 22
_isPrefixOf :: Eq a => [a] -> [a] -> Bool
-- _isPrefixOf [10,20] [10,20,30] -> True
-- _isPrefixOf [10,30] [10,20,30] -> False
_isPrefixOf [] _ = True
_isPrefixOf _ [] = False
_isPrefixOf (h1:t1) (h2:t2) | h1 == h2 = _isPrefixOf t1 t2
                            | otherwise = False

-- questão 23
_isSuffixOf :: Eq a => [a] ->[a] -> Bool
-- _isSuffixOf [20,30] -> [10,20,30] -> True
-- _isSuffixOf [10,30] -> [10,20,30] -> False
_isSuffixOf [] _ = True
_isSuffixOf _ [] = False
_isSuffixOf l1 (h:t) | l1 == (h:t) = True
                     | otherwise = _isSuffixOf l1 t

-- questão 24
_isSubsquenceOf :: Eq a => [a] -> [a] -> Bool
-- _isSubsquenceOf [20,30] [10,20,30,40] -> True
-- _isSubsquenceOf [40,20] [10,20,30,40] -> False
_isSubsquenceOf [] _ = True
_isSubsquenceOf _ [] = False
_isSubsquenceOf (h1:t1) (h2:t2) | h1 == h2 = _isSubsquenceOf t1 t2
                                | otherwise = _isSubsquenceOf (h1:t1) t2

-- questão 25
_elemIndices :: Eq a => a -> [a] -> [Int]
-- _elemIndices 3 [1,2,3,4,3,2,3,4,5] -> [2,4,6]
_elemIndices _ [] = []
_elemIndices n l = elemIndicesAux 0 n l

elemIndicesAux :: Eq a => Int -> a -> [a] -> [Int]
elemIndicesAux _ _ [] = []
elemIndicesAux i n (h:t) | n == h = i : elemIndicesAux (i+1) n t
                         | otherwise = elemIndicesAux (i+1) n t

-- questão 26
_nub :: Eq a => [a] -> [a]
-- _nub [1,2,1,2,3,1,2] -> [1,2,3]
_nub [] = []
_nub l = nubAux l []

nubAux :: Eq a => [a] -> [a] -> [a]
nubAux [] resp = resp
nubAux (h:t) resp | (elem h resp) = nubAux t resp
                  | otherwise = nubAux t (resp ++ [h])

-- questão 27
_delete :: Eq a => a -> [a] -> [a]
-- _delete 2 [1,2,1,2,3,1,2] -> [1,1,2,3,1,2]
_delete _ [] = []
_delete x (h:t) | x == h = t
                | otherwise = h : _delete x t

-- questão 28
_remove :: Eq a => [a] -> [a] -> [a]
-- _remove [1,2,3,4,5,1] [1,5] -> [2,3,4,1]
_remove l [] = l
_remove [] _ = []
_remove (h1:t1) (h2:t2) | h1 == h2 = _remove t1 t2
                        | otherwise = h1 : _remove t1 (h2:t2)

-- questão 29
_union :: Eq a => [a] -> [a] -> [a]
-- _union [1,1,2,3,4] [1,5] -> [1,1,2,3,4,5]
_union [] l2 = l2
_union l1 [] = l1
_union l1 (h2:t2) | (elem h2 l1) = _union l1 t2
                  | otherwise = _union (l1 ++ [h2]) t2

-- questão 30
_intersect :: Eq a => [a] -> [a] -> [a]
-- _intersect [1,1,2,3,4] [1,3,5] -> [1,1,3]
_intersect [] _ = []
_intersect l1 [] = l1
_intersect (h1:t1) l2 | (elem h1 l2) = h1 : _intersect t1 l2
                      | otherwise = _intersect t1 l2

-- questão 31
_insert :: Ord a => a -> [a] -> [a]
-- _insert 25 [1,20,30,40] -> [1,20,25,30,40]
_insert x [] = [x]
_insert x (h:t) | x <= h = x : h : t
                | otherwise = h : _insert x t

-- questão 32
_unwords :: [String] -> String
-- _unwords ["Progamacao","Funcional"] -> "Programacao Funcional"
_unwords [] = ""
_unwords [x] = "x"
_unwords (h:t) = h ++ _unwords t

-- questão 33
_unlines :: [String] -> String
-- _unlines ["Prog","Func"] -> "Prog\nFunc\n"
_unlines [] = ""
_unlines (h:t) = h ++ "\n" ++ _unlines t

-- questão 34
pMaior :: Ord a => [a] -> Int
pMaior [] = 0
pMaior (h:t) = pMaiorAux 0 0 h t

pMaiorAux :: Ord a => Int -> Int -> a -> [a] -> Int
pMaiorAux i curr x [] = i
pMaiorAux i curr x (h:t) | x < h = pMaiorAux (curr + 1) (curr + 1) h t
                         | otherwise = pMaiorAux i (curr + 1) x t

-- questão 35
_lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- _lookup 'a' [('a',1),('b',4),('c',5)] -> Just 1
_lookup _ [] = Nothing
_lookup a ((h1,h2):t) | a == h1 = Just h2
                      | otherwise = _lookup a t

-- questão 36
preCrescente :: Ord a => [a] -> [a]
-- preCrescente [3,7,9,10,22] -> [3,7,9]
preCrescente [] = []
preCrescente (h:t) | h < head t = h : preCrescente t
                   | otherwise = preCrescente t

-- questão 37
iSort :: Ord a => [a] -> [a]
iSort [] = []



-- questão 38
menor :: String -> String -> Bool
-- menor "sai" "saiu" -> True
-- menor "progamacao" "funional" -> False
menor [] _ = True
menor _ [] = False
menor (h1:t1) (h2:t2) | h1 < h2 = True
                      | h1 == h2 = menor t1 t2
                      | otherwise = False

-- questão 39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
-- elemMSet 'a' [('b',2),('a',4),('c',1)] -> True
-- elemMSet 'd' [('b',2),('a',4),('c',1)] -> False
elemMSet _ [] = False
elemMSet x ((h1,h2):t) | x == h1 = True
                       | otherwise = elemMSet x t

-- questão 40
converteMSet :: [(a,Int)] -> [a]
-- converteMSet [('b',2),('a',4),('c',1)] -> "bbaaaac"
converteMSet [] = []
converteMSet ((h1,0):t) = converteMSet t
converteMSet ((h1,h2):t) = [h1] ++ converteMSet ((h1,(h2-1)):t)

-- questão 41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
-- insereMSet 'c' [('b',2),('a',4),('c',1)] -> [('b',2),('a',4),('c',2)]
insereMSet x [] = [(x,1)]
insereMSet x ((h1,h2):t) | x == h1 = (h1,h2+1) : t
                         | otherwise = insereMSet x t

-- questão 42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
-- removeMSet 'c' [('b',2),('a',4),('c',1)] -> [('b',2),('a',1)]
removeMSet _ [] = []
removeMSet x ((h1,h2):t) | x == h1 && h2 == 1 = t
                         | x == h1 = (h1,h2-1) : t
                         | otherwise = (h1,h2) : removeMSet x t

-- questão 43
constroiMSet :: Ord a => [a] -> [(a,Int)]
-- constroiMSet "aaabccc" -> [('a',3),('b',1),('c',3)]
constroiMSet [] = []


-- questão 44
-- _partitionEithers :: [Either a b] -> ([a],[b])
