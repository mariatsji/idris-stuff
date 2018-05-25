import data.Vect

zip' : Vect n a -> Vect n b -> Vect n (a,b)
zip' [] ys = []
zip' (x :: xs) (y :: ys) = (x, y) :: zip xs ys

-- function syntax
-- Nat and successor
-- lists / cons
-- pattern matching
-- let / in


insert : Ord a => (x : a)
               -> (xsSorted : Vect len a)
               -> Vect (S len) a
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs else y :: insert x xs

insSort : Ord a => Vect n a -> Vect n a
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs
                    in (insert x xsSorted)


unzip : Vect n (a,b) -> (Vect n a, Vect n b)
unzip [] = ([], [])
unzip ((x,y)) :: xs) = ?unzip_rhs_2
