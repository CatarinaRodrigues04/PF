data Hora = H Int Int 
          deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

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

