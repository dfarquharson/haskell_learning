data List t = E | C t (List t)
  deriving Show

lst1 :: List Int
lst1 = C 3 $ C 5 $ C 2 E

lst2 :: List Char
lst2 = C 'x' $ C 'y' $ C 'z' E

lst3 :: List Bool
lst3 = C True $ C False E

filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head $ tail xs)

doStuff2 :: [Int] -> Int
doStuff2 []         = 0
doStuff2 [_]        = 0
doStuff2 (x1:x2:_)  = x1 + x2

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safehead (x:_) = Just x

data NonEmptyList a = NEL a [a]
  deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNel :: NonEmptyList a -> [a]
tailNel (NEL _ as) = as
