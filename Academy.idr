import data.Vect

zip' : Vect n a -> Vect n b -> Vect n (a,b)
zip' [] ys = []
zip' (x :: xs) (y :: ys) = (x, y) :: zip' xs ys

insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                     else y :: (insert x xs)

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let
                      xsSorted = insSort xs
                    in
                      insert x xsSorted

trans : (f : a -> b) -> Vect n a -> Vect n b
trans f [] = []
trans f (x :: xs) = f x :: trans f xs

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right) = let leftfold =  foldr func acc left
                                           rightfold = foldr func leftfold right in
                                           func e rightfold
