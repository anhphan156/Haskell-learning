doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                    then x
                    else x*2
boomBang xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs , odd x]
-- length' xs = sum [1|_<-xs]
removeNonUppercase st = [c|c<-st,c`elem`['A'..'Z']]

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

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi w h = w / h ^ 2

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "1 element: " ++ show x
tell (x:y:[]) = "2 elements: " ++ show x ++ ", " ++ show y
tell (x:y:_) = "Many elements, first one is: " ++ show x ++ ", second one is: " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital [] = "Empty String"
capital all@(x:xs) = "First letter of " ++ all ++ " is: " ++ [x]