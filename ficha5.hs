
import Data.List
import Data.Char
--import System.Random 

type Polinomio = [Monomio]
type Monomio = (Float,Int)


selgrau :: Int -> Polinomio -> Polinomio
selgrau x l = filter (filtrarpolinomio x) l
       where 
           filtrarpolinomio x (c,e) = (x==e)

--filtrarpolinomio :: Int -> Monomio -> Bool
--filtrarpolinomio x (c,e) = (x==e)

contagrau :: Int -> Polinomio -> Int
contagrau x l = sum(map (contapolinomio x) l)
          where
            contapolinomio x (c,e) = if (x==e) then 1 else 0 

contagrau1 :: Int -> Polinomio -> Int
contagrau1 x l = foldl (+) 0 ( map (contapolinomio x)  l)
           where
             contapolinomio x (c,e) = if (x==e) then 1 else 0




grau :: Polinomio -> Int
grau l = foldl maxGrau 0 l
       where
           maxGrau acc (c,e) = if acc > e then acc else e

grau2 :: Polinomio -> Int
grau2 l = maximum (map (retirarExp ) l)
        where 
            retirarExp (c,e) = e 

simp :: Polinomio -> Polinomio
simp l = filter (retirazero ) l
        where
            retirazero (c,e) = (c/=0) 

mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) l = map (multAux (x,y) ) l 
        where 
            multAux (x,y) (c,e) = (x*c,y+e)

ordena :: Polinomio -> Polinomio
ordena l = sortOn snd l

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = (foldl (normAux) h  t) : normaliza (filter (retiExp h) t)
    where
       normAux (c,e) (x,y) = if (e==y) then (c+x,y) else (c,e) 
       retiExp (c,e) (x,y) = (e/=y)


data Exp a = Const a
  | Simetrico (Exp a)
  | Mais (Exp a) (Exp a)
  | Menos (Exp a) (Exp a)
  | Mult (Exp a) (Exp a)


instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico b) = "-(" ++ show b ++ ")"
    show (Mais b c) = "(" ++ show b ++ ") + (" ++ show c ++ ")"
    show (Menos b c) = "(" ++ show b ++ ") - (" ++ show c ++ ")"
    show (Mult b c ) = "(" ++ show b ++ ") * (" ++ show c ++ ")"

data BTree a = Empty | Node a (BTree a) (BTree a)

instance Show a => Show (BTree a) where
    show Empty = "*"
    show (Node n l r) = "(" ++ show l ++ "<-" ++ show n ++ "->" ++ show r ++ ")"

a1 = Node 5 (Node 3 Empty Empty)(Node 7 Empty (Node 9 Empty Empty))

--bingo :: IO ()
--bingo = do 
--     putStrLn "Pressione qualquer tecla"
--     numero <- randomRIO (1,90)
--     putStrLn (show numero)


elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x l = auxElem 0 x l 

auxElem :: Eq a => Int -> a -> [a] -> [Int]
auxElem _ _ [] =  []
auxElem i x (h:t) = if (x==h) then i : auxElem (i+1) x t else auxElem (i+1) x t 


isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (x:y) = if (h==x) then isSubsequenceOf' t y else isSubsequenceOf' (h:t) y

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP x Empty = Nothing
lookupAp x (Node (a,b) l r) | x==a = Just b
                            | x<a = lookupAP x l
                            | otherwise = lookupAP x r

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node a esq dir) (Node b left right) = Node (f a b) (zipWithBT f esq left) (zipWithBT f dir right)



digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) = if isDigit x then (x:a,b)
                    else (a,x:b) 
    where (a,b) = digitAlpha xs

--Ex 4 e 5 ??? 2018-1019

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)
            deriving Eq


n = (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil))

firstSeq :: Eq a => Seq a -> a
firstSeq (Cons a s) = a
firstSeq (App s1 s2) = if (s1==Nil) then firstSeq s2 else firstSeq s1

firstSeq2 :: Eq a => Seq a -> a
firstSeq2 (Cons a s) = if (s==Nil) then a else firstSeq2 s
firstSeq2 (App s1 s2) = if (s2==Nil) then firstSeq2 s1 else firstSeq2 s2 

dropSeq :: Eq a => Int -> Seq a -> Seq a
dropSeq 0 s = s 
dropSeq n (Cons a s) = dropSeq (n-1) s
dropSeq n (App s1 s2) = if (s1==Nil) then dropSeq n s2 else App (dropSeq n s1) s2

instance Show a => Show (Seq a) where
       show s = auxShow True s

auxShow :: (Show a) => Bool -> Seq a -> String
auxShow _ (Nil) = ""
auxShow b (Cons a s) = show a ++ "," ++ auxShow b s  
auxShow b (App s1 s2) = if b then  "<<" ++ auxShow False s1 ++ auxShow False s2 ++ ">>" else auxShow b s1 ++ auxShow b s2 
 

--Exame 2019

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [h] = True
isSorted (h:h1:t) = if (h<=h1) then isSorted (h1:t) else False

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--2) ???
maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB [] = Nothing
maximumMB [h] = h 
maximumMB ((Just a): (Just b) :t) =if a<b then maximumMB ((Just b):t) else maximumMB ((Just a):t)
maximumMB (Nothing: h1:t) = maximumMB (h1:t)
maximumMB (h: Nothing: t) = maximumMB (h:t)




instance (Show a) => Show (LTree a) where
    show (Tip a) = show a ++ "\n"
    show (Fork a b) = mostra 1 a ++ mostra 1 b

mostra :: (Show a) => Int -> LTree a -> String
mostra n (Tip a) = replicate n '.' ++ show a ++ "\n"
mostra n (Fork a b) = mostra (n + 1) a ++ mostra (n + 1) b

data LTree a = Tip a | Fork (LTree a) (LTree a)

a2 = Fork (Fork (Tip 7) (Tip 1)) (Tip 2)

a3 = Tip 3

--4
