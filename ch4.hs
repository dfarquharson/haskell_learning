-- this is function composition at work
foo f g = \x -> f $ g x

--myTest :: [Integer] -> Bool
--myTest xs = even $ length $ greaterThan100 xs

--foobar :: [Integer] -> Integer
--foobar []     = 0
--foobar (x:xs)
--  | x > 3     = (7*x +2) foobar xs
--  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)

sum''     = fold 0 (+)
product'' = fold 1 (*)
length''  = fold 0 (\_ s -> 1 + s)
