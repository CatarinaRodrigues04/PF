import Data.Char

-- 1. Defina as seguintes funções e os respetivos tipos:

-- a) perimetro -- que calcula o perímetro de uma circunferência, dado o comprimento do seu raio.
perimetro :: Float -> Float
perimetro r = 2 * pi * r

-- b) dist - que calcula a distância entre dois pontos no plano Cartesiano. Cada ponto
-- é um par de valores do tipo Double.
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- c) primUlt - que recebe uma lista e devolve um par com o primeiro e o último
-- elemento dessa lista.
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

-- d) multiplo - tal que a multiplo m n testa se o número inteiro m é multiplo de n
multiplo :: Int -> Int -> Bool
multiplo m n | mod m n == 0 = True
             | otherwise = False

-- e) truncaImpar - que recebe uma lista e, se o comprimento da lista for ímpar 
-- retira-lhe o primeiro elemento, caso contrário devolve a própria lista.
truncaImpar :: [a] -> [a]
truncaImpar l | mod (length l) 2 == 0 = l
              | otherwise = tail l

-- f) max2 - uqe calcula o maior de dois números inteiros
max2 :: Int -> Int -> Int
max2 a b | a > b = a
         | otherwise = b

-- g) max3 - que calcula o maior de três números inteiros, usando a função max2
max3 :: Int -> Int -> Int -> Int
max3 a b c | a >= b && a >= c = a
           | b >= a && b >= c = b
           | otherwise = c

-- 2. Feina as seguintes funções obre polinómios de 2º grau:

-- a) A função nRaizes que recebe os (3) coeficientes de um polinímio de 2º grau e que 
-- calulca a lista das suas raízes (reais) desse polinómio
nRaizes :: Float -> Float -> Float -> Int
nRaizes x y z | delta < 0 = 0
              | delta == 0 = 1
              | delta > 0 = 2
            where delta = (y^2) - 4 * x * y

-- b) A função raizes que, usando a função anterior, recebe os coeficientes do polinómio
-- e calcula a lista das suas raízes reais.
raizes :: Float -> Float -> Float -> [Float]
raizes x y z | nRaizes x y z == 0 = []
             | nRaizes x y z == 1 = [root]
             | nRaizes x y z == 2 = [root, root1]
            where
                root = ((-y) + sqrt((y^2) - 4 * x * z)) / (2*x)
                root1 = ((-y) - sqrt((y^2) - 4 * x * z)) / (2*x)

-- 3. Vamos representar horas por um par de números inteiros:
type Hora = (Int, Int)
-- Assim o par (0,15) significa meia noite e um quarto e (13, 45) duas menos um quarto.
-- Defina funções para:

-- a) testar se um par de inteiros representa uma hora do dia válida;
validTime :: Hora -> Bool
validTime (h,m) | h >= 0 && h < 24 && m >= 0 && m < 60 = True
                | otherwise = False

--b) testar se uma hora é ou não depois de outra (comparação);
compareTime :: Hora -> Hora -> Bool
compareTime (h1,m1) (h2,m2) | h1 > h2 = True
                            | h1 == h2 && m1 > m2 = True
                            | otherwise = False

-- c) converter um valor em horas (par de inteiros) para minutos (inteiro);
hourToMinutes :: Hora -> Int
hourToMinutes (h,m) = (h * 60) + m

-- d) converter um valor em minutos para horas;
minutesToHours :: Int -> Hora
minutesToHours m = (div m 60, mod m 60)

-- e) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos);
timeBetween :: Hora -> Hora -> Int
timeBetween t1 t2 = abs(hourToMinutes t1 - hourToMinutes t2)

-- f) adicionar um determinado número de minutos a uma dada hora;
addMinutes :: Hora -> Int -> Hora
addMinutes t m = minutesToHours(hourToMinutes t + m)

-- 4. Repita o exercício anterior assumindo agora que as horas são representadas por um
-- novo tipo de dados:
data Hora2 = H Int Int deriving (Show,Eq)
-- Com este novo tipo a hora meia noite e um quarto é representada por H 0 15 e a hora
-- duas menos um quarto por H 13 45

-- a) testar se um par de inteiros representa uma hora do dia válida;
validTime2 :: Hora2 -> Bool
validTime2 (H h m) | h >= 0 && h < 24 && m >= 0 && m < 60 = True
                   | otherwise = False

-- b) testar se uma hora é ou não depois da outra (comparação)
compareTime2 :: Hora2 -> Hora2 -> Bool
compareTime2 (H h1 m1) (H h2 m2) | h1 > h2 = True
                                 | h1 == h2 && m1 > m2 = True
                                 | otherwise = False

-- c) coverter um valor em horas para minutos
hourToMinutes2 :: Hora2 -> Int
hourToMinutes2 (H h m) = (h*60) + m

-- d) converter um valor em minutos para horas
minutesToHours2 :: Int -> Hora2
minutesToHours2 m = (H (div m 60) (mod m 60))

-- e) calcular a diferença entre duas horas (reultado em minutos)
timeBetween2 :: Hora2 -> Hora2 -> Int
timeBetween2 t1 t2 = abs(hourToMinutes2 t1 - hourToMinutes2 t2)

-- f) adicionar um determinado número de minutos a uma dada hora
addMinutes2 :: Hora2 -> Int -> Hora2
addMinutes2 t m = minutesToHours2(hourToMinutes2 t + m)

-- 5. Considere o seguinte tipo de dados para representar os possíveis estados de um semáforo:
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a) Defina a função next :: Semaforo -> Semaforo que calcula o próximo estado de um semáforo.
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b) Defina a função stop :: Semaforo -> Bool que determina se é obrigatório parar num semáforo.
stop :: Semaforo -> Bool
stop Verde = False
stop Amarelo = False
stop Vermelho = True

-- c) Defina a função safe :: Semaforo -> Semaforo -> Bool que testa se o estado de dois semáforos
-- num cruzamenteo é seguro.
safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

-- 6. Um ponto num plano pode ser representado por um sistema de coordenadas Cartesiano
-- (distâncias aos eixos vertical e horizontal) ou por um sistema de coordenadas Polar
-- (distância à origem e ângulo do respetivo vetor com o eixo horizontal).
data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)
-- Com este tipo o ponto Cartesiano (-1) 0 pode alternativamente ser representado por
-- Polar 1 pi. Defina as seguintes funções:

-- a) posx :: Ponto -> Double que calcula a distância de um ponto ao eixo vertical.
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar dist angle) = abs(dist * (cos angle))

-- b) posy :: Ponto -> Double que calcula a distância de um ponto ao eixo horizontal.
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar dist angle) = abs(dist * (sin angle))

-- c) raio :: Ponto -> Double que calcula a distância de um ponto à origem.
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar dist angle) = dist

-- d) angulo :: Ponto -> Double que calcula o ângulo entre o vetos que liga a origem
-- ao ponto e o eixo horizontal.
angulo :: Ponto -> Double
angulo (Cartesiano x y) | x > 0 = atan(y / x)
                        | x < 0 && y >= 0 = atan(y / x) + pi
                        | x < 0 && y < 0 = atan(y / x) - pi
                        | x == 0 && y > 0 = pi / 2
                        | x == 0 && y < 0 = -(pi / 2)
angulo (Polar dist angle) = angle

-- e) distancia :: Ponto -> Ponto -> Double que calcula a distância entre dois pontos.
distancia :: Ponto -> Ponto -> Double
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
distancia (Cartesiano x y) (Polar dist angle) = sqrt((posx (Polar dist angle) - x)^2 + (posy (Polar dist angle) - y)^2)
distancia (Polar dist angle) (Cartesiano x y) = sqrt((x - posx(Polar dist angle))^2 + (y - posy(Polar dist angle))^2)
distancia (Polar dist1 angle1) (Polar dist2 angle2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
                                        where
                                            x1 = posx(Polar dist1 angle1)
                                            x2 = posx(Polar dist2 angle2)
                                            y1 = posy(Polar dist1 angle1)
                                            y2 = posy(Polar dist2 angle2)

-- 7. Considere o seguinte tipo de dados para representar figuras geométricas num plano.
data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

-- O tipo de dados diz que uma figura pode ser um círculo centrado num determinado
-- ponto e com um determinado raio, um retângulo paralelo aos eixos representado por
-- dois pontos que são értices da sua diagonal, ou um triângulo representado pelos trẽs
-- pontos dos seus vértices. Defina as seguintes funções:

-- a) Defina a função poligno :: Figura -> Bool que testa se uma figura é um polígno.
poligno :: Figura -> Bool
poligno (Circulo _ _) = False
poligno _ = True

-- b) Defina a função vertices :: Figura -> [Ponto] que calcula a lista dos vértices de uma figura.
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, p2, (Cartesiano (posx p1) (posy p2)), (Cartesiano (posx p2) (posy p1))]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

-- c) Complete a seguinte definição cujo objetivo é calcular a área de uma figura:
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = distancia p1 p2
        b = distancia p2 p3
        c = distancia p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
-- ...
area (Circulo p1 r) = pi * r^2
area (Retangulo p1 p2) = abs((posx p1 - posx p2) * (posy p1- posy p2))

-- d) Defina a função perimetro :: Figura -> Double que calcula o perímetro de uma figura
perimetroF :: Figura -> Double
perimetroF (Circulo p r) = 2 * pi * r
perimetroF (Retangulo p1 p2) = (abs(posx p1 - posx p2) * 2) + (abs(posy p1 - posy p2) * 2)
perimetroF (Triangulo p1 p2 p3) = let a = distancia p1 p2
                                      b = distancia p2 p3
                                      c = distancia p3 p1
                                      in a + b + c

-- 8. Utilizando as funções ord :: Char -> Int e chr :: Int -> Char do módulo Data.Char,
-- defina as seguintes funções:

-- a) _isLower :: Char -> Bool, que testa se um Char é uma minúscula.
_isLower :: Char -> Bool
_isLower x | val <= 122 && val >= 97 = True
           | otherwise = False
                where val = ord x

-- b) isDigit :: Char -> Bool, que testa se um Char é um dígito.
_isDigit :: Char -> Bool
_isDigit x | val <= 57 && val >= 48 = True
          | otherwise = False
            where val = ord x

-- c) isAlpha :: Char -> Bool, que testa se um Char é uma letra.
_isAlpha :: Char -> Bool
_isAlpha x | val <= 122 && val >= 97 || val <= 90 && val >= 65 = True
          | otherwise = False
            where val = ord x

-- d) toUpper :: Char -> Char, que converte uma letra para a respetiva maiúscula
_toUpper :: Char -> Char
_toUpper x = chr ((ord x) - 32)

-- e) intToDigit :: Int -> Char, que converte um número entre 0 e 9 para o respetivo dígito.
intToDigit :: Int -> Char 
intToDigit x = chr (x + 48)

-- f) digitToInt :: Char -> Int, que converte um dígito para o respetivo inteiro.
digitToInt :: Char -> Int
digitToInt x = (ord x) - 48 