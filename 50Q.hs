--1

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y = if (x>y) then [] else x : enumFromTo1 (x+1) y 

--2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z | x > z && y > x || x < z && y < x || x == y && x > z = []
                      | otherwise = x : enumFromThenTo1 y (2*y-x) z

--3
concatena1 :: [a] -> [a] -> [a]
concatena1 [] l = l
--concatena1 l [] = l
concatena1 (x:y) l = x: concatena1 y l

--4
indice1 :: [a] -> Int -> a
indice1 (h:t) x = if (x==0) then h else indice1 t (x-1)

--5
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

--6
take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 n (h:t)
              | n <= 0 = []
              | otherwise = h : take1 (n-1) t

--7
drop1 :: Int -> [a] -> [a]
drop1 _ [] = []
drop1 n (h:t) | n <= 0 = (h:t)
              |otherwise = drop1 (n-1) t

--8
zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x:y) : zip1 xs ys

--9
replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x 
               | n < 0 = []
               | otherwise x : replicate1 n (x-1)  

--10
intersperse1 :: a -> [a] -> [a]
intersperse1 x [] = []
intersperse1 x [y] = [y]
intersperse1 x (h:t) = h : x : intersperse1 x t

--11
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 (h:t) = (h:takeWhile (== h) t) : group1 (dropWhile (== h) t)

--12
concat :: [[a]] -> [a]
concat [] = []
concat (h:t) = h ++ concat t

--13
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = inits1 (init l) ++ [l]

--14
tails1 :: [a] -> [[a]]
tails1 [] = [[]]
tails1 l@(h:t) = l : tails1 (tail l)

--15
heads :: [[a]] -> [a]
heads [] = []
heads ([]:t) = heads t
heads (h:t) = auxHeads h : heads t
       where auxHeads [a] -> a
             auxHeads [] = []
             auxHeads (h:t) = h

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = subTotal h + total t
    where subTotal :: [a] -> Int
          subTotal [] = 0
          subTotal (h:t) = 1 + subTotal t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

--18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):t) = a ++ cola t

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade a i [] = []
idade a i ((n,an):t) | (a-an) >= i = n : idade a i t
                     | otherwise = idade a i t

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m
    | m > 1 = powerEnumFrom n (m - 1) ++ [n^(m-1)]
    | otherwise = []

--21
isPrime :: Int -> Bool
isPrime n
    | n >= 2 = primeCheck n 2
    | otherwise = False

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m + 1)

--22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) =  x == y && isPrefixOf xs ys                           

--23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf _ [] = False
isSuffixOf [] _ = True
isSuffixOf (x:xs) (y:ys) = (x:xs) (y:ys) || isSuffixOf (x:xs) ys

--24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = y==x && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys

--25
elemIndices :: Eq a => a -> [a] -> [Int]   
elemIndices x [] = []
elemIndices x l = auxElem x 0 l
            where 
            auxElem :: Eq a => a -> a -> [a] -> [Int]   
            auxElem x c [] = []
            auxElem x c (h:t) | x==h = c : auxElem x c+1 t
                              | otherwise aux x c+1

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if h `elem` t then nub' t else h : nub' t

--27
delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t) = if (x==h) then t else h : delete x t

--28
(\\):: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) _ [] = _
(\\) l (h:t) = remove (delete h l) t

--29
union :: Eq a => [a] -> [a] -> [a]
union l [] = l
union l (h:t)
    | h `elem` l = union l t
    | otherwise = union (l ++ [h]) t

--30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (h:t) l
    | h `elem` l = h : intersect t l
    | otherwise = intersect t l

--31
insert :: Ord a => a -> [a] -> [a]
insert  x [] = [x]
insert x (h:t) = if (h>=x) then x : (h:t) else h : insert x t

--32
unwords :: [String] -> String
unwords [] = ""
unwords (h:t) = h ++ (if null t then "" else " ") ++ unwords t

--33
unline :: [String] -> String
unlines [] = ""
unlines (h:t) = h ++ "\n" ++ unwords t

--34
pMaior :: Ord a => [a] -> Int
pMaior [_] = 0
pMaior (h:t)
    | h >= (t !! x) = 0
    | otherwise = 1 + x
    where x = pMaior t

--35
lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup e ((a,b):t)
    | e == a = Just b
    | otherwise = lookup e t

--36


preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t)
    | s >= h = h : preCrescente (s:t)
    | otherwise = [h]

--37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert h (iSort t)

--38
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (h':t')
    | h < h' = True
    | h == h' = menor t t'
    | otherwise = False

--39
elemMSet ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,y):xs) = a == x || elemMSet a xs

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,1):xs) = x : converteMSet xs
converteMSet ((x,n):xs) = x : converteMSet ((x,n-1) : xs)

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,n):xs) = if x == a then (a,n+1) : xs else (a,n) : insereMSet x xs

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,n):xs) 
    | x == a = if n > 1 then (a, n-1) : xs else xs
    | otherwise = (a,n) : removeMSet x xs

--43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)

--44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (e:es) = case e of 
                        Left a -> ((a:x),y)
                        Right b -> (x,(b:y))
                        where (x,y) = partitionEithers es

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) = case m of Nothing -> catMaybes ms
                             Just x -> x : catMaybes ms

--46
caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (xi, yi) (xf, yf) 
    | xi < xf = Este : caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste : caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte : caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul : caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

--47
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x, y) (m:ms) = posicao (case m of Norte -> (x, y + 1)
                                           Sul -> (x, y - 1)
                                           Este -> (x + 1, y)
                                           Oeste -> (x - 1, y)) ms

hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops pi ms = pi == posicao pi ms || hasLoops pi (init ms)

--48
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) 
    | eQuadrado h = 1 + contaQuadrados t
    | otherwise = contaQuadrados t

eQuadrado :: Rectangulo -> Bool
eQuadrado (Rect (x1,y1) (x2,y2)) = abs (y2 - y1) == abs (x2 - x1)

--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs (x2 - x1) * abs (y2 - y1) + areaTotal t

--50
naoReparar' :: [Equipamento] -> Int
naoReparar' [] = 0
naoReparar' (h:t) = case h of Avariado -> naoReparar' t
                              _ -> 1 + naoReparar' t











 