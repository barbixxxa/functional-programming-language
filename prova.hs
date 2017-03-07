-- 1º Questao
--a)
seguinte :: Int -> Int
seguinte x | x >= maxBound = error "Numero muito grande para ser Int"
           | otherwise = succ x

--b)
data Day = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Eq, Enum, Show, Bounded)


diaSeguinte :: Day -> Day
diaSeguinte n | n == maxBound = minBound
              | otherwise = succ n

--2º Questao              
