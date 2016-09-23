                              --12/08

-- comentario em uma linha
{-
comentario
em bloco
-}

incr :: Int -> Int 
incr x = x+1

decr :: Int -> Int
decr a = a -1

fran :: Int
fran = 24

soma :: Int -> Int
soma c = c + fran 

produto :: (Int,Int) -> Int
produto (e,f) = e*f

incr_decr :: Int -> (Int,Int)
incr_decr x = (x+1,x-1)

nivel::Int -> Bool
nivel h = h == fran


                                
                                --18/08
                                
raiz :: (Float,Float,Float) -> (Float,Float)
raiz (a,b,c) = ((-b+sqrt((b^2)-(4*a*c)))/(2*a),(-b-sqrt((b^2)-(4*a*c)))/(2*a))


enesimo :: (Int,Int,Int) -> Int
enesimo (a,n,r) = a + (n-1)*r

pgnesimo :: (Int,Int,Int) -> Int
pgnesimo (a,n,q) = a*q^(n-1)

fib:: Int -> Int
fib n = if(n == 1 || n==2 || n==0) then 1 else (fib(n-1) + fib(n-2))

binNew :: Int -> Int -> Int
binNew l c = if (c==0) then 1
    else if (c==l) then 1
    else ((binNew(l-1) c) + (binNew(l-1)(c-1)))

binNewton :: Int -> Int -> Int
binNewton l c| c==0 = 1
             | c==l = 1
             | otherwise = ((binNewton(l-1) c) + (binNewton(l-1) (c-1) ))

binN :: Int -> Int -> Int -- outro modo de fazer
binN l 0 = 1
binN l c
        |l == c = 1
        |otherwise = binN(l-1) c + binN(l-1) (c-1)

sominha :: [Int] -> Int
sominha l = if l == [] then 0
            else(head l) + sominha(tail l)

sominha2 :: [Int] -> Int
sominha2 l | l==[] = 0
           |otherwise = (head l) + sominha2 (tail l)

sominha3 :: [Int] -> Int
sominha3 [] = 0
sominha3 (h:t) = h + sominha3 t

produtoLista :: [Int] -> Int
produtoLista n = if n == [] then 1
                 else(head n) * produtoLista(tail n)



-- gerar uma lista de pa e pg com razao, a1 e an
paLista :: (Int, Int, Int) -> [Int]
paLista (a,r,an)   |a == an = [a] 
                   |(a + r > an) = [a]
                   |otherwise = [a] ++ paLista(a+r,r,an)


                              --25/08
                      
-- produto de todos os elementos de uma lista

pgLista :: (Int, Int, Int) -> [Int]
pgLista (a, r, an) |a==an = [a]
                   |(a * r > an) = [a]
                   |otherwise = [a] ++ pgLista(a*r,r,an)

-- gerar uma lista de inteiros a partir de uma lista de listas de inteiros


-- inverter uma lista
invert :: [Int] -> [Int]
invert [] = []
invert [n] = [n]
invert (h:t) = (invert t) ++[h]

{-inverterLista :: [Int] -> [Int]
inverterLista [n] | [n] == [] = []
                  | [n] == [n] = [n] --pq nao roda?
                  | otherwise = inverterLista tail ++ head-}


fib2:: Int -> Int
fib2 n  |(n == 1 || n==2 || n==0) = 1
        |otherwise = (fib(n-1) + fib(n-2))

fat1:: Int -> Int
fat1 n | n == 0 = 1
       |otherwise = n*fat1 (n-1) --diferentes

fat2 :: Int -> Int
fat2 0 = 1
fat2 n = ((n) * fat2 (n-1)) --tipos de

fat3 :: Int -> Int
fat3 n = if n == 0 then 1 else ((n)* fat3 (n-1)) -- fazer a mesma funcao



--Recursivade sem cauda

inverterSemCauda :: [Int] -> [Int]
inverterSemCauda n = inverterAux n []

inverterAux :: [Int] -> [Int] -> [Int]
inverterAux [] n = n
inverterAux (h:t) l = inverterAux t (h:l) 
                                            
                                    -- 26/08/2016
tam::[a]->Int
tam [] = 0
tam(h:t) = 1+tam t

somaA:: Num a => [a] -> a
somaA [] = 0
somaA(h:t)= h + somaA t

{-class Num a where
    (+)::a->a->a
    (-)::a->a->a
    (*)::a->a->a

instance Num Int where
    x+y=primitive-}

                                       --Monitoria 02/09
palavras :: String -> String -> [String]
palavras [] a = [a]
palavras (h:t) a = if (h==' ') then a:(palavras t [])
                   else palavras t (a ++ [h])

checarPalavrasAux :: [String] -> String -> Bool
checarPalavrasAux [] p = False
checarPalavrasAux (h:t) p = if (h==p) then True
                            else checarPalavrasAux t p  
checarPalavras :: [String] -> [String] -> [String]
checarPalavras dicionario [] = []
checarPalavras dicionario (h:t) = if (checarPalavrasAux dicionario h) then checarPalavras dicionario t
                                  else h:(checarPalavras dicionario t)

--linhas :: Int -> [String] -> [[String]]

                                      --Monitoria 08/09
{-Questões
1) Programa que receba uma lista de parametro e retorne apenas o ultimo elemento
2) Reescreva o codigo aceima para o penultimo
3)Faça um programa que identifique se uma lista, passada por parametro, é um palindromo
4)Faça um programa que elimine as repetições de uma lista-}

ultimoElemento :: [a] -> a
ultimoElemento [a] = a                    --Se for uma lista de um só elemento, retorna um unico elemento
ultimoElemento (h:t) = ultimoElemento t   -- Retorna o ultimo elemento da lista

penultimoElemento :: [a] -> a
penultimoElemento [a, b] = a                     --Se for uma lista de apenas dois elementos, retorna o penultimo elemento
penultimoElemento (h:t) = penultimoElemento t    --Retorna o penultimo elemento da lista

penultimoElemento2 :: [a] -> a                   --Feito por calazans
penultimoElemento2 (h:t) = aux (h:t) h           -- Passa para a função aux a lista e seu head
              where aux [b] a = a                --
                    aux (h:t) _ = aux t h        --


palindromo :: (Eq a) => [a] -> Bool
palindromo list = aux list (revert list)
         where aux :: (Eq a) => [a] -> [a] -> Bool
               aux [] _ = True
               aux (h1:t1) (h2:t2)| (h1==h2) = aux t1 t2
                                  |otherwise = False
               revert :: [a] -> [a]
               revert [] = []
               revert (h:t) = (revert t) ++ [h]

elimRep :: (Eq a) => [a] -> [a]
elimRep [] = []                                                        --retorna uma lista vazia
elimRep [a] = [a]                                                      --retorna uma lista de um só elemento
elimRep (h:t) = if(h == (head t)) then elimRep t else h:(elimRep t) --Compara o primeiro item da lista com o proximo item da lista
                            --Caso nao seja igual, concatena o head com o que retornar da funçao
                                                                       
{- Feito por calazans, nao ta rodando
eliminarRep :: (Eq a)=> [a] -> [a]
eliminarRep a = a [] 
     where aux :: (Eq a) => [a] -> [a] -> [a]
           aux [] a = a
           aux (h:t) a | (contem h a) = aux t a
                       | otherwise = aux t (h:a)
           contem :: (Eq a) => a -> [a] -> Bool
           contem a [] = False
           contem a (h:t) = h==a | (contem a t)
-}

                                           
                                           -- 15/09/2016
media :: [Int] -> Int
media l = div(somaM l) (length l) where
              somaM :: [Int] -> Int
              somaM [] = 0 
              --somaM (h:t) = h + somaM t
              somaM l = foldl (+) 1 l

               
              {-

repetidos :: [Int] -> [Int]
repetidos [] = []
repetidos [a] = [a]
repetidos (h:t) = h:(repetidos (removeTodos h t)) where
                               removeTodos :: a -> [a] -> [a]
                               removeTodos _ [] = []
                               removeTodos n (h:t) = if(n==h) then removeTodos n t else (h:removeTodos n t)


repetidos2 :: [Int] -> [Int]
repetidos2 [] = []
repetidos2 [a] = [a]
repetidos2 (h:t) = h:(repetidos2 (removeTodos h t)) where
                               removeTodos :: a -> [a] -> [a]
                               removeTodos _ [] = []
                               removeTodos n (h:t) = (if(n==h) then [] else [h]) ++ removeTodos n t

repetidos3 :: [Int] -> [Int]
repetidos3 [] = []
repetidos3 [a] = [a]
repetidos3 (h:t) = h:(repetidos3 (removeTodos h t)) where
                               removeTodos :: a -> [a] -> [a]
                               removeTodos n l = filter (\x -> x/=n) l
repetidos4 :: [Int] -> [Int]
repetidos4 [] = []
repetidos4 [a] = [a]
repetidos4 (h:t) = h:(repetidos3 (removeTodos h t)) where
                               removeTodos :: a -> [a] -> [a]
                               removeTodos n l = [x | x <- l, x/= n]

-}

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (h:t) = quickSort ( filter (<h) t ) ++ [h] ++ filter (>=h) t
{- Filtra os valores menores que o head, que é o pivô, concatena com o head e depois com os maiores que o pivô-}

qsort :: [Int] -> [Int]
qsort []  = []
qsort (h:t) = qsort [y | y<-t, y<h] ++ [h] ++ qsort [y | y<-t, y>h]

bubbleSort :: [Int] -> [Int] --by: Eduardo Müller
bubbleSort [] = []
bubbleSort [a] = [a]
bubbleSort l | (verificar l) = l
             | otherwise = bubbleSort (trocar l) where 
                                                  trocar :: [Int] -> [Int]
                                                  trocar [] = []
                                                  trocar [a] = [a]
                                                  trocar (h:t) | (h > head t) = (head t):(trocar (h:(tail t)))
                                                               | otherwise = h:(trocar t)

                                                  verificar :: [Int] -> Bool
                                                  verificar [a] = True
                                                  verificar (h:t) | (h < head t) = verificar t
                                                                  | otherwise = False
   
insertionSort :: [Int] -> [Int] --by: Eduardo Müller
insertionSort [] = []
insertionSort [a] = [a]
insertionSort l | (verificar l) = l
                | otherwise = insertionSort (comparar [] l) where 
                                        comparar :: [Int] -> [Int] -> [Int]
                                        comparar [] [] = []
                                        comparar l [] = l
                                        comparar [] (h:t) = comparar [h] t 
                                        comparar (h1:t1) (h2:t2) | (h1 < h2) = (h1:comparar t1 (h2:t2))
                                                                 | otherwise = (h2:comparar (h1:t1) t2)
                                        verificar :: [Int] -> Bool
                                        verificar [a] = True
                                        verificar (h:t) | (h <= head t) = verificar t
                                                        | otherwise = False



selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort [a] = [a]
selectionSort (h:t) | (h < minimum t) = h:(selectionSort t)--verifica se o head é maior que o menor numero da lista, caso seja concatena a lista ao head
                    | otherwise = (minimum (h:t)):(selectionSort (h:t)) -- caso contrario, o head é o menor numero da lista e será concatenado a lista




mergeSort :: [Int] -> [Int] --by: Marcos Vilela, Daniel Vasconcellos, Eduardo Müller e Renato
mergeSort [a] = [a]
mergeSort [] = []
mergeSort l = merge (mergeSort(dividir l)) (mergeSort(dividil l)) where 

                                                                    dividir :: [Int] -> [Int]--by: Marcos Vilela, Daniel Vasconcellos, Eduardo Müller e Renato
                                                                    dividir [a] = [a]
                                                                    dividir l = drop (length l `div` 2) (l)

                                                                    dividil :: [Int] -> [Int]--by: Marcos Vilela, Daniel Vasconcellos, Eduardo Müller e Renato
                                                                    dividil [a] = [a]
                                                                    dividil l = take (length l `div` 2) (l)

                                                                    merge :: [Int] -> [Int] -> [Int]--by: Marcos Vilela, Daniel Vasconcellos, Eduardo Müller e Renato
                                                                    merge [] [] = []
                                                                    merge [] (h:t) = (h:t)
                                                                    merge (h:t) [] = (h:t)
                                                                    merge (h1:t1) (h2:t2) | h1 > h2 = h2:(merge (h1:t1) t2)
                                                                                          | otherwise = h1:(merge t1 (h2:t2))

{-update :: Int -> [Int] -> [Int]
update _ [] = []
update n l | (n <= length l)-}

                                     --22/09/2016

qSort :: (Int -> Int -> Bool) -> [Int] -> [Int] -- se executa assim: qSort (\ x y -> x > y) [lista]
qSort _ [] = [] --by: Marcos Vilela
qSort _ [a] = [a]
qSort f (h:t) = (qSort f [y | y<-t, (f h y)]) ++ [h] ++ (qSort f [y | y<-t, not(f h y)])

qSort2 :: (Int -> Int -> Bool) -> [Int] -> [Int] --outro jeito de fazer
qSort2 f l = qsortaux l where 
               qsortaux [] = []
               qsortaux [a] = [a]
               qsortaux (h:t) = (qsortaux [x | x <- t, (f h x)]) ++ [h] ++ (qsortaux [x | x <- t, not(f h x)])



selecSort :: [Int] -> [Int] -- by: Daniel Vasconcellos
selecSort [] = []
selecSort [a] = [a]
selecSort l = (minimo l):(selecSort (removeN l (minimo l))) where
                                                              minimoL :: [Int] -> Int -> Int -- by: Daniel Vasconcellos
                                                              minimoL [] a = a
                                                              minimoL (h:t) 0 = minimoL t h
                                                              minimoL [a] h | a < h = a
                                                                            | otherwise = h
                                                              minimoL (h:t) a | a < h = minimoL t a
                                                                              | otherwise = minimoL t h
                                                              minimo :: [Int] -> Int -- by: Daniel Vasconcellos
                                                              minimo [] = 0
                                                              minimo l = minimoL l 0

                                                              removeN :: [Int] -> Int -> [Int] -- by: Daniel Vasconcellos
                                                              removeN [] a = []
                                                              --removeN [a] b = []
                                                              removeN (h:t) a | a == h = t
                                                                              | otherwise = h:(removeN t a) 
--sSort :: (Int -> Int -> Bool) -> [Int] -> [Int]