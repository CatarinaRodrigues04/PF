module ExameTreino where

import Data.List
import Data.Char


-- Árvores Binárias (Ficha 6 / Exames)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- Sequências com inserção nas pontas (Exame 2016/2017)
data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a deriving Show

-- Movimentos de um Robot (Exame 2017/2018 / Ficha 9)
data Movimento = Norte | Sul | Este | Oeste deriving Show

-- Matrizes (Exame 2017/2018)
type Mat a = [[a]]

{- 
   PARTE 1: Listas e Recursividade 
-}

-- 1. (Exame 2016/2017)
-- A função (\\) retorna a lista resultante de remover da primeira lista 
-- a *primeira ocorrência* de cada elemento da segunda lista.
-- Ex: diferenca [1,2,3,4,5,1] [1,5] == [2,3,4,1]
myDifference :: (Eq a) => [a] -> [a] -> [a]
myDifference [] _ = []
myDifference l [] = l
myDifference l (h:t) = myDifference (_delete h t) t

_delete :: (Eq a) => a -> [a] -> [a]
_delete _ [] = []
_delete x (h:t) | x == h = t
               | otherwise = h : _delete x t 

-- 2. (50 Questões - Q.22)
-- Testa se a primeira lista é prefixo da segunda.
-- Ex: isPrefixOf [10,20] [10,20,30] == True
-- Ex: isPrefixOf [10,30] [10,20,30] == False
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (h1:t1) (h2:t2) | h1 == h2 = myIsPrefixOf t1 t2
                             | otherwise = False

-- 3. (50 Questões - Q.11)
-- Agrupa elementos iguais e consecutivos de uma lista.
-- Ex: group [1,2,2,3,4,4,4,5,4] == [[1],[2,2],[3],[4,4,4],[5],[4]]
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (h:t) = (h : takeWhile (==h) t) : myGroup (dropWhile (==h) t)

{- 
   PARTE 2: Árvores Binárias e Tipos Algébricos 
-}

-- 4. (Exame 2016/2017)
-- Remove de uma árvore todos os elementos a partir de uma determinada profundidade.
-- Se a profundidade for 0, a árvore fica vazia (Empty).
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node n l r) = Node n (prune (x-1) l) (prune (x-1) r)

-- 5. (Exame 2016/2017 - Seq)
-- Recebe uma sequência não vazia e devolve a sequência sem o seu *último* elemento.
-- Nota: Tens de tratar os construtores Inicio e Fim.
semUltimo :: Seq a -> Seq a
semUltimo (Inicio a Nil) = Nil
semUltimo (Inicio a t) = Inicio a (semUltimo t)
semUltimo (Fim a t) = a

-- 6. (Ficha 6)
-- Dado um caminho (False = esquerda, True = direita) e uma árvore, 
-- dá a lista com a informação dos nodos por onde esse caminho passa.
path :: [Bool] -> BTree a -> [a]
path [] (Node n l r) = [n]
path _ Empty = []
path (h:t) (Node n l r) | h = n : path t r
                        | otherwise = n : path t l


{- 
   PARTE 3: Manipulação e Lógica (Exames e Fichas Variadas) 
-}

-- 7. (Exame 2017/2018)
-- Dada uma posição inicial (x,y) e uma lista de movimentos, calcula a posição final.
-- Norte: y+1, Sul: y-1, Este: x+1, Oeste: x-1
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao 

-- 8. (Exame 2018/2019)
-- Calcula o maior elemento de uma lista de Maybe a.
-- Considere Nothing o menor dos elementos.
-- Ex: maximumMB [Just 2, Nothing, Just 5] == Just 5
maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB = undefined

-- 9. (Exame 2017/2018)
-- Testa se uma matriz quadrada é triangular superior 
-- (i.e., todos os elementos abaixo da diagonal principal são nulos/zero).
-- Dica: Podes usar funções de ordem superior ou recursividade.
triSup :: (Num a, Eq a) => Mat a -> Bool
triSup = undefined