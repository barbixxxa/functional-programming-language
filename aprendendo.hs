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

                                            --23/09/2016
                                            -- Execução Parcial
ordena_ascendente :: [Int] -> [Int]                                          
ordena_ascendente  = qSort2 (\ x y -> (x>y))

somaCurry :: Int -> Int -> Int   -- Equivalentes
somaCurry x y = x + y

somaCurry' :: Int -> (Int -> Int) --Notação
somaCurry' x = \ y -> x + y

somaCurry'' = \ x y -> x + y  --Curry

soma' :: (Int, Int) -> Int --Função não curry
soma' (x,y) = x+y

--curry :: ((a,b) -> c) -> (a -> b -> c)
--uncurry:: (a -> b -> c) -> ((a,b) -> c)

--Questões em classe até a 1b. O resto para casa
--1) Escreva funções para
--    a)Incrementar os valores de uma lista de inteiros 
--       Ex: incrementar [1,2,3] é igual [2,3,4]
inc = map (\x -> x+1)

--    b)Somar 2 listas
--       Ex: somaL [1,2,3] [4,5,6] é igual [5,7,9]
somaL x y = map (uncurry (+)) (zip x y)

--2) Fazer recursivo e em alta ordem
--    a) all [True,True,False] = False
allTodos :: [Bool] -> Bool --By: Eduardo Müller
allTodos [] = False
allTodos [a] = True
allTodos (h:t) | (h == head t) = allTodos t
               | otherwise = False


allTodosAO :: [Bool] -> Bool --By: Eduardo Müller
allTodosAO b = comparaBoolAO (==) b where 
                                      comparaBoolAO :: (Bool -> Bool -> Bool) -> [Bool] -> Bool 
                                      comparaBoolAO _ [] = False
                                      comparaBoolAO _ [a] = a
                                      comparaBoolAO f (h:t) = f h (comparaBoolAO f t)

--    b) some [True,False,True] = True
algum :: [Bool] -> Bool -> Bool--By: Eduardo Müller
algum [] _ = False
algum (h:t) b | (h == b) = True
              | otherwise = algum t b

algum2 :: [Bool] -> Bool--By: Eduardo Müller  
algum2 [] = False
algum2 [a] = False
algum2 (h:t) | (h /= (head t)) = True
             | otherwise = algum2 t

algumAO :: [Bool] -> Bool--By: Eduardo Müller
algumAO b = comparaBoolAO (/=) b where
                                  comparaBoolAO :: (Bool -> Bool -> Bool) -> [Bool] -> Bool 
                                  comparaBoolAO _ [] = False
                                  comparaBoolAO _ [a] = a
                                  comparaBoolAO f (h:t) = f h (comparaBoolAO f t)

--    c) andL [True,False,True] [True,True,False] = [True,False,False]
andL :: [Bool] -> [Bool] -> [Bool]--By: Eduardo Müller
andL [] _ = []
andL _ [] = []
andL [a] [b] = (a && b):[]
andL (h1:t1) (h2:t2) | (h1 == h2) = True:(andL t1 t2)
                     | otherwise = False:(andL t1 t2)

andL2 :: [Bool] -> [Bool] -> [Bool]--By: Eduardo Müller
andL2 [] _ = []
andL2 _ [] = []
andL2 [a] [b] = (a && b):[]
andL2 (h1:t1) (h2:t2) = (h1 && h2):(andL2 t1 t2)

andLAO :: [Bool] -> [Bool] -> [Bool] --By: Eduardo Müller
andLAO a b = altaOrdem (&&) a b where
                                 altaOrdem :: (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> [Bool]
                                 altaOrdem _ _ [] = []
                                 altaOrdem _ [] _ = []
                                 altaOrdem f [a] [b] = (f a b):[]
                                 altaOrdem f (h1:t1) (h2:t2) = (f h1 h2):(altaOrdem f t1 t2)

--    d) orL [True,False] [False,True] = [True, True]
orL :: [Bool] -> [Bool] -> [Bool] --By: Eduardo Müller
orL [] _ = []
orL _ [] = []
orL [a] [b] = (a || b):[]
orL (h1:t1) (h2:t2) = (h1 || h2):(orL t1 t2)

orLAO :: [Bool] -> [Bool] -> [Bool] --By: Eduardo Müller
orLAO a b = altaOrdem (||) a b where
                                 altaOrdem :: (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> [Bool]
                                 altaOrdem _ _ [] = []
                                 altaOrdem _ [] _ = []
                                 altaOrdem f [a] [b] = (f a b):[]
                                 altaOrdem f (h1:t1) (h2:t2) = (f h1 h2):(altaOrdem f t1 t2)



                           --06/10/2016
fet 0 = 1  --haskell identifica qual o tipo a ser usado
fet x = x*(fet(x-1))


                           --Avaliação tardia
{-
   Exemplos:
 seja f x y = x + y
f (1+2) (2+3) => (1+2) + (2+3) => 3 + 5 => 8

 seja g x y = if (x>0) then x else y
g (1+2) (2+3) => x = (1+2) = 3 > 0 => retorna 3, nao precisei calcular y. Poupei tempo.

g 2 (div 3 0) => x = 2 > 0 => retorna 2, nao precisei calcular y que daria um erro por dividir por 0.

g (g (1+2) (4+1)) (g -1 (2+3)) => precisa calcular o x do g de fora => g (1+2) (4+1) => x = (1+2) = 3 > 0 => retorna 3.
                               g 3 (g -1 (2+3)) => x = 3 > 0 => retorna 3, nao precisei calcular o y. 

-}


-- Os argumentos de uma funcao nao sao avaliados antes da execucao da funcao.
-- A avaliacao destes eh postergarda o maximo possivel.

{-
Vantagens:
 Computação infinita
  Exemplos:
   seq x = x:(seq(x+1)) => seq 0 = [0,1,2,3,4...]
   
   take 2 (seq 0) => take 2 (0:seq(0+1)) => 0:(take (2-1) (seq (0+1))) => 0:(take 1 (seq (0+1)))
                  => 0:(take 1 ((0+1):seq (0+1+1))) => 0:(0+1):(take (1-1)) (seq (0+1+1)) 
                  => 0:(0+1):(take 0 seq (0+1+1)) => 0:(0+1):[] => 0:1:[]
                  => [0,1]

-}


                    --07/10/2016

{- Lista de exercicios de avaliação tardia

a) Naturais:
nat = 0:(sucessor nat) where
  sucessor(h:t) = (h+1):(sucessor t)

    nat = n1
    n1 = 0:sucessor n1 = 0:n2
    n2 = sucessor n1 = (0+1):(sucessor n2) = (1):(n3)
    n3 = sucessor n2 = (1+1):(sucessor n3) = 2:n4
    n4 = sucessor n3 = (2+1):(sucessor n4) = 3:n5

b) Fibonacci:
fib = 1:1:(soma fib f1) where
  soma (h1:t1) (h2:t2) = (h1 + h2):(soma t1 t2)
f1 = tail fib

  fib = 1:1:n1
  n1 = soma (fib (tail fib)) = soma (1:1:n1 1:n1) = (1+1):(soma 1:n1 n1) = (2):n2
  n2 = soma(1:n1 n1) = (1 + n1):(soma 2:n2 n2) = (1 + 2:n2):soma(2:n2 n2) = (1+)

-}

									--28/10/2016
data Exp = Num Int | X
         | Soma Exp Exp
         | Produto Exp Exp
         | Subt Exp Exp
         | Div Exp Exp
         | Pot Exp Int
         deriving Show
--Exemplo: avalie( Soma (Num 2) (Produto (Num 3) (Num 4)))

avalie :: Exp -> Int -> Int
avalie (Num n) x = n
avalie (Soma e1 e2) x = (avalie e1 x) + (avalie e2 x)
avalie (Produto e1 e2) x = (avalie e1 x) * (avalie e2 x)
avalie X x = x

-- Ex2: avalie (Soma X (Num 8)) 5

derivada:: Exp -> Exp --Computação simbolica
derivada (Num n) = Num 0
derivada X = Num 1
derivada (Soma e1 e2) = Soma (derivada e1) (derivada e2)

derivada' :: (Float -> Float) -> (Float -> Float)
derivada' f x = (f(x+h) - f x)/h where h = 0.001

--ex2 Soma (Pot x 2) (Produto (Num 2) X)
--ex3 = derivada ex2
-- h = derivada' (avalie ex2)
-- i = avalie (derivada ex2)
--l0 = map (avalie ex2) [1.0,1.1..10]
--l1 = map h [1.0,1.1..10] 
--l2 = map i [1.0,1.1..10]