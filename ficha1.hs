--exercicio 1

perimetro :: Int -> Int
perimetro r = 2*pi*r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x,y) (xs,ys) = sqrt ((xs - x)^2 + (ys - y)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo m n | mod m n == 0 = True
             | otherwise = False

truncaImpar :: [a] -> [a]
truncaImpar l | mod length l 2 == 0 = l
              | otherwise = tail l

max2 :: Int -> Int -> Int
max2 x y | x > y = x
         | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

--exercicio 2

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c | delta < 0 = 0
              | delta == 0 = 1
              | delta > 0 = 2
            where delta = b² - 4 * a * c

raizes :: Float -> Float -> Float ->[Float]
raizes a b c | (nRaizes a b c) == 0 = []
             | (nRaizes a b c) == 1 = [raiz]
             | (nRaizes a b c) == 2 = [raiz2]
            where 
                raiz = ((-b + sqrt(y² - 4*a*c)) / 2*a)
                raiz2 = ((-b - sqrt(y² - 4*a*c)) / 2*a)

--exercicio 3

type Hora = (Int,Int)
-- par (0,15) - meia noite e um quarto  par (13,45) - duas menos um quarto

horaVal :: Hora -> Bool
horaVal (h,m) | 0 <= h && h < 24 && m >= 0 && m < 60 = True
              | otherwise = False

afterThen :: Hora -> Hora -> Bool
afterThen (h1,m1) (h2,m2) | h1 > h2 = True
                          | h1 == h2 && m1 > m2 = True
                          | otherwise = False

hourToMinutes :: Hora -> Int
hourToMinutes (h,m) = (h*60) + m

minutesToHour :: Int -> Hora
minutesToHour m = (div m 60, mod m 60)

timeBetween :: Hora -> Hora -> Int
timeBetween t1 t2 = abs((hourToMinutes t1) - (hourToMinutes t2))

addMinutes :: Hora -> Int -> Hora
addMinutes h m = minutesToHour((hourToMinutes t + m))

--exercicio 4

data Hora = H Int Int deriving (Show,Eq)
-- meia noite e meia e um quarto - H 0 15  duas menos um quarto - H 13 45

validHour :: Hora -> Bool
validHour (H h m) | h >= 0 && h < 24 && m >= 0 && m < 60 = True
                | otherwise = False
            
afterThen2 :: Hora -> Hora -> Bool
afterThen2 (H h1 m1) (H h2 m2) | h1 > h2 = True
                           | h1 == h2 && m1 > m2 = True
                           | otherwise = False

hourToMinutes2 :: Hora -> Int
hourToMinutes2 (H h m) = (h*60) + m

minutesToHour2 :: Int -> Hora
minutesToHour2 m = (H (div m 60) (mod m 60))

timeBetween2 :: Hora -> Hora -> Int
timeBetween2 t1 t2 = abs ((hourToMinutes2 t1) - (hourToMinutes2 t2))

addMinutes2 :: Hora -> Int -> Hora
addMinutes2 h m = minutesToHour2(hourToMinutes2 h + m)

--exercicio 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Verde = False
stop Amarelo = False
stop Vermelho = True

safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True
safe Vermelho _ = True
safe _ _ = False

--exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double 
            deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar dist angle) = dist * (cos angle)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar dist angle) 0 dist * (sin angle)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x² + y²)
raio (Polar dist angle) = dist

angulo :: Ponto -> Double
angulo (Cartesiano x y) | x > o = atan(y/x)
                        | x < 0 && y > 0 = atan(y/x) + pi
                        | x < 0 && y < 0 = atan(y/x) - pi
                        | x == 0 && y > 0 = pi/2
                        | x == 0 && y < 0 = -pi/2
angulo (Polar dist angle) = angle

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)
dist (Cartesiano x y) (Polar d a) = sqrt ((posx (Polar d a) - x)^2 + (posy (Polar d a) - y)^2)
dist (Polar d a) (Cartesiano x y) = sqrt ((x - posx (Polar d a))^2 + (y - posy (Polar d a))^2)
dist (Polar d1 a1) (Polar d2 a2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
                            where x2 = posx (Polar d2 a2)
                                  x1 = posx (Polar d1 a1)
                                  y2 = posy (Polar d2 a2)
                                  y1 = posy (Polar d1 a1)

--exercicio 7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

poligno :: Figura -> Bool
poligno Circulo _ _ = False
poligno _ _ = True

vertices :: Figura -> [Ponto]
vertices Circulo _ _ = []
vertices (Retangulo x y) = 


--exercicio 8

_isLower :: Char -> Bool
_isLower x | val <= 122 && val >= 97 = True
           | otherwise = False


_isDigit :: Char -> Bool
_isDigit x | elem x ['0'..'9'] = True
           | otherwise = False

_isAlpha :: Char -> Bool
_isAlpha x | (val <= 122 && val >= 97) || (val <= 90 && val >= 65) = True
           | otherwise = False

_toUpper :: Char -> Char
_toUpper x = chr((ord x) - 32)

_intToDigit :: Int -> Char
_intToDigit x