myButLast :: [a] -> a
myButLast = last . init

{-pegaElem :: [a] -> Int -> a
pegaElem [] n = []
pegaElem (x:xs) 0 = x
pegaElem (x:xs) n = xs pegaElem (n-1)
-}

numeros :: String -> String
numeros "90" = "Noventa"
numeros "9" = "Nove"
numeros ('9':t) = "Noventa e " ++ numeros t
numeros "80" = "Oitenta"
numeros "8" = "Oito"
numeros ('8':t) = "Oitenta e " ++ numeros t
numeros "7" = "Sete"
numeros "70" = "Setenta"
numeros ('7':t) = "Setenta e " ++ numeros t
numeros "60" = "Sesenta"
numeros "6" = "Seis"
numeros ('6':t) = "Sessenta e " ++ numeros t
numeros "50" = "Ciquenta"
numeros "5" = "Cinco"
numeros ('5':t) = "Ciquenta e " ++ numeros t
numeros "40" = "Quarenta"
numeros "4" = "Quatro"
numeros ('4':t) = "Quarenta e " ++ numeros t
numeros "30" = "Trinta"
numeros "3" = "Tres"
numeros ('3':t) = "Trinta e " ++ numeros t
numeros "20" = "Vinte"
numeros "2" = "Dois"
numeros ('2':t) = "Vinte e " ++ numeros t
numeros "1" = "Um"
numeros "11" = "Onze"
numeros "10" = "Dez"
numeros "12" = "Doze"
numeros "13" = "Treze"
numeros "14" = "Catorze"
numeros "15" = "Quinze"
numeros "16" = "Dezesseis"
numeros "17" = "Dezessete"
numeros "18" = "Dezoito"
numeros "19" = "Dezenove"
numeros "0" = "Zero"
numeros a = a

beer a |(a == 1) =  putStrLn ((numeros (show a)) ++ " garrafa de cerveja na parede \n" ++ (numeros (show a))  ++ " garrafa de cerveja na parede. \nPegue uma garrafa! Beba a garrafa.\n") >> beer (a-1)
       |(a == 0) =  putStrLn "Nenhuma garrafa de cerveja na parede e a musica acabou \n"
       |True =  putStrLn ((numeros (show a)) ++ " garrafas de cerveja na parede \n" ++ (numeros (show a))  ++ " garrafas de cerveja. \nPegue uma garrafa! Beba a garrafa!\n") >> beer (a-1)

main = beer 99