module EparaPassar where

import Data.Char
import Data.List 


--1. Apresente uma definição recursiva da função (pré-definida) enumFromTo :: Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites.

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y | x >= y = x : []
                 | x < y = x : myenumFromTo (x+1) y
                 | otherwise = []

--2. Apresente uma definição recursiva da função (pré-definida) enumFromThenTo :: Int -> Int -> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites e espaçados de um valor constante.

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z | x >= z = x : []
                       | x < z = x : myenumFromThenTo y ( y + ( y - x ) ) z
                       | otherwise = []

--3. Apresente uma definição recursiva da função (pré-definida) (++) :: [a] -> [a] -> [a] que concatena duas listas.

(+++) :: [a] -> [a] -> [a]
(+++) x [] = x
(+++) [] x = x
(+++) (x:xs) l = x : ((+++) xs l) 

{-4. Apresente uma definição recursiva da função (pré-definida) (!!) :: [a] -> Int -> a que dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (assumese que o primeiro elemento se 
encontra na posição 0). -}

(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) n = (!!!) t (n-1) 

--5. Apresente uma definição recursiva da função (pré-definida) reverse :: [a] -> [a] que dada uma lista calcula uma lista com os elementos dessa lista pela ordem inversa.

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]

--6. Apresente uma definição recursiva da função (pré-definida) take :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l.

mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (h:t) = if n > 0 then h : mytake (n-1) t else []

--7. Apresente uma definição recursiva da função (pré-definida) drop :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l.

mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop 0 l = l
mydrop n (h:t) = if n > 0 then mydrop (n-1) t else (h:t)

--8. Apresente uma definição recursiva da função (pré-definida) zip :: [a] -> [b] -> [(a,b)] constrói uma lista de pares a partir de duas listas.

myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x , y) : myzip xs ys
myzip _ _ = []


--9. Apresente uma definição recursiva da função (pré-definida) elem :: Eq a => a -> [a] -> Bool que testa se um elemento ocorre numa lista.

myelem :: Eq a => a -> [a] -> Bool
myelem x [] = False
myelem x (h:t) = if x == h then True else myelem x t


--10. Apresente uma definição recursiva da função (pré-definida) replicate :: Int -> a -> [a] que dado um inteiro n e um elemento x constrói uma lista com n elementos, todos iguais a x.

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n l = if n > 0 then l : myreplicate (n-1) l else []

{-11. Apresente uma definição recursiva da função (pré-definida) intersperse :: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido é intercalado entre os elementos da 
lista fornecida. -}

myintersperse :: a -> [a] -> [a]
myintersperse x [] = []
myintersperse x [a] = [a]
myintersperse x (h:t) = h : x : myintersperse x t 

--12. Apresente uma definição recursiva da função (pré-definida) group :: Eq a => [a] -> [[a]] que agrupa elementos iguais e consecutivos de uma lista.

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h:t) = (h : takeWhile (==h) t) : mygroup (dropWhile (==h) t)

--13. Apresente uma definição recursiva da função (pré-definida) concat :: [[a]] -> [a] que concatena as listas de uma lista. 

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t

--14. Apresente uma definição recursiva da função (pré-definida) inits :: [a] -> [[a]] que calcula a lista dos prefixos de uma lista.

myinits::[a]->[[a]]
myinits [] = [[]]
myinits l  = myinits (init l) ++ [l]

--15. Apresente uma definição recursiva da função (pré-definida) tails :: [a] -> [[a]] que calcula a lista dos sufixos de uma lista.

mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = l : mytails (tail l)

--16. Apresente uma definição recursiva da função (pré-definida) isPrefixOf :: Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra.

myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _ = True
myisPrefixOf _ [] = False
myisPrefixOf (x:xs) (y:ys) = x == y && myisPrefixOf xs ys

--17. Apresente uma definição recursiva da função (pré-definida) isSuffixOf :: Eq a => [a] -> [a] -> Bool que testa se uma lista é sufixo de outra.

myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = True
myisSuffixOf _ [] = False
myisSuffixOf l1 l2 = isPrefixOf (reverse l1) (reverse l2)

--18. Apresente uma definição recursiva da função (pré-definida) isSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa.

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False
myisSubsequenceOf (x:xs) (y:ys) = if x == y then myisSubsequenceOf xs ys else myisSubsequenceOf (x:xs) ys  

--19. Apresente uma definição recursiva da função (pré-definida) elemIndices :: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um dado elemento ocorre numa lista.

myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices x [] = []
myelemIndices x l = aux 0 x l
                    where aux i x [] = []
                          aux i x (h:t) = if x == h then (i) : aux (i+1) x t else aux (i+1) x t 


--20. Apresente uma definição recursiva da função (pré-definida) nub :: Eq a => [a] -> [a] que calcula uma lista com os mesmos elementos da recebida, sem repetições.

mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (h:t) = h : filter (/= h) (mynub t)


--21. Apresente uma definição recursiva da função (pré-definida) delete :: Eq a => a -> [a] -> [a] que retorna a lista resultante de remover (a primeira ocorrência) de um dado elemento de uma lista.

mydelete :: Eq a => a -> [a] -> [a]
mydelete x [] = []
mydelete x (h:t) = if x == h then t else h : mydelete x t

--22. Apresente uma definição recursiva da função (pré-definida) (\\):: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira.

(\\\):: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ = []
(\\\) l (h:t) = (\\\) (delete h l) t  

--23. Apresente uma definição recursiva da função (pré-definida) union :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de acrescentar à primeira lista os elementos da segunda que não ocorrem na primeira.

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] l = l
myunion l [] = l
myunion l (h:t) = if elem h l then myunion l t else myunion (l++[h]) t


--24. Apresente uma definição recursiva da função (pré-definida) intersect :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda.

myintersect :: Eq a => [a] -> [a] -> [a]
myintersect [] _ = []
myintersect (h:t) l = if elem h l then h : myintersect t l else myintersect t l

--25. Apresente uma definição recursiva da função (pré-definida) insert :: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.

myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert x (h:t) = if x < h then x:h:t else h:myinsert x t


--26. Apresente uma definição recursiva da função (pré-definida) unwords :: [String] -> String que junta todas as strings da lista numa só, separando-as por um espaço.

myunwords :: [String] -> String
myunwords [] = ""
myunwords [x] = x
myunwords (x:xs) = x ++ " " ++ myunwords xs

--27. Apresente uma definição recursiva da função (pré-definida) unlines :: [String] -> String junta todas as strings da lista numa só, separando-as pelo caracter ’\n’.

myunlines :: [String] -> String
myunlines [] = ""
myunlines [x] = x
myunlines (x:xs) = x ++ "\n" ++ myunlines xs


{-28. Apresente uma definição recursiva da função pMaior :: Ord a => [a] -> Int que dada uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista. As posições da lista começam em 0, i.e., 
a função deverá retornar 0 se o primeiro elemento da lista for o maior. -}

mypMaior :: Ord a => [a] -> Int
mypMaior (h:t) = aux 0 0 h t
             where aux i im _ [] = im
                   aux i im x (y:ys) = if x < y then aux (i+1) (i+1) y ys else aux (i+1) im x ys    

--29. Apresente uma definição recursiva da função temRepetidos :: Eq a => [a] -> Bool que testa se uma lista tem elementos repetidos. 

mytemRepetidos :: Eq a => [a] -> Bool
mytemRepetidos [] = False
mytemRepetidos (h:t) = elem h t || mytemRepetidos t


--30.  Apresente uma definição recursiva da função algarismos :: [Char] -> [Char] que determina a lista dos algarismos de uma dada lista de caracteres.

myalgarismos :: [Char] -> [Char]
myalgarismos = filter (`elem` ['0'..'9']) 


--31. Apresente uma definição recursiva da função posImpares :: [a] -> [a] que determina os elementos de uma lista que ocorrem em posições ímpares. Considere que o primeiro elemento da lista ocorre na posição 0 e por isso par.

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [a] = []
posImpares (a:b:t) = b:posImpares t

--32. Apresente uma definição recursiva da função posPares :: [a] -> [a] que determina os elementos de uma lista que ocorrem em posições pares. Considere que o primeiro elemento da lista ocorre na posição 0 e por isso par.

posPares :: [a] -> [a]
posPares [] = []
posPares [a] = [a]
posPares (a:b:t) = a:posPares t

--33. Apresente uma definição recursiva da função isSorted :: Ord a => [a] -> Bool que testa se uma lista está ordenada por ordem crescente.

myisSorted :: Ord a => [a] -> Bool
myisSorted [] = True
myisSorted [x] = True
myisSorted (x:y:xs) = if x > y then False else myisSorted (y:xs) 

{-34. Apresente uma definição recursiva da função iSort :: Ord a => [a] -> [a] que calcula o resultado de ordenar uma lista. Assuma, se precisar, que existe definida a função insert :: Ord a => a -> [a] -> [a] que dado um elemento 
e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista. -}

myiSort :: Ord a => [a] -> [a]
myiSort [] = []
myiSort (h:t) = insert h (myiSort t)


--35. Apresente uma definição recursiva da função menor :: String -> String -> Bool que dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo a ordem lexicográfica (i.e., do dicionário)

menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (x:xs) (y:ys) = x < y || menor xs ys