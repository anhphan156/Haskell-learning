-- Baby functions
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                    then x
                    else x*2

-- List comprehension
boomBang xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs , odd x]

removeNonUppercase st = [c|c<-st,c`elem`['A'..'Z']]

-- Pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Out of luck"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Integral a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "1 element: " ++ show x
tell (x:y:[]) = "2 elements: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "Many elements, first one is: " ++ show x ++ ", second one is: " ++ show y

-- length' xs = sum [1|_<-xs]

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital [] = "Empty String"
capital all@(x:xs) = "First letter of " ++ all ++ " is: " ++ [x]

-- Guards

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--     | bmi <  18.5 = "Skinny"
--     | bmi < 25.0 = "Ordinary"
--     | bmi < 30.0 = "Fatass"
--     | otherwise = "..."

-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell w h
--     | w / h ^ 2 < 18.5 = "Skinny"
--     | w / h ^ 2 < 25.0 = "Ordinary"
--     | w / h ^ 2 < 30.0 = "Fatass"
--     | otherwise = "..."

-- Where binding

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
    | bmi < skinny = "Skinny"
    | bmi < ordinary = "Ordinary"
    | bmi < fatass = "Fatass"
    | otherwise = "..."
    where bmi = w / h ^ 2
          skinny = 18.5
          ordinary = 25.0
          fatass = 30.0  

initials :: String -> String -> String
initials first last = "Initials are: " ++ [f] ++ " " ++ [l]
    where (f:_) = first
          (l:_) = last

calcBmis :: (RealFloat a) => [(a,a)] -> [String]
calcBmis xs = [bmiTell (bmi w h) | (w,h) <- xs]
    where bmi w h = w / h ^ 2
          bmiTell a
                  | a < 18 = "Skinny"
                  | a < 25 = "Ordinary"
                  | a < 30 = "Fat"
                  | otherwise = "..."

-- Let it be

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- Recursion
equalString :: String -> String -> Bool
equalString [] [] = True
equalString (x:xs) (y:ys) = x == y && equalString xs ys
equalString _ _ = False

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail 
    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' x y
    | x <= 0 = []
    | otherwise = y : replicate' (x-1) y

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' x _
    | x <= 0 = []
take' _ [] = []
take' x (y:ys) = y : take' (x - 1) ys

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: (Num a) => a -> [a]
repeat' a = a:repeat' a

zip' :: [a] -> [b] ->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq i) => i -> [i] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = x `elem'` ys

quicksort :: (Ord i) => [i] -> [i]
quicksort [] = []
quicksort (x:xs) = 
    let smallPart = quicksort [a | a <- xs, a <= x]
        greatPart = quicksort [a | a <- xs, a > x]
    in
        smallPart ++ [x] ++ greatPart

-- Curried funtions

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- High order functions

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = [] 
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x 

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x == True = x : again
    | otherwise = again
    where again = filter' f xs

largestDivisible :: (Integral a) => [a]
largestDivisible = filter' p [100000,99999..0]
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2) 
    | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length' (filter' isLong (map chain [1..100]))
    where isLong x = length' x > 15


sum'' :: (Num i) => [i] -> i
sum'' = foldl (+) 0 

elem'' :: (Eq i) => i -> [i] -> Bool
elem'' x xs = foldl (\acc y -> if y == x then True else acc) False xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

maximum'' :: Ord a => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)
