module Main

main : IO ()
main = putStrLn "hi"

stringOrInt : Bool -> Type
stringOrInt b =
    case b of
         True => Int
         False => String

getStringOrInt : (x : Bool) -> stringOrInt x
getStringOrInt x =
    case x of
         True => 94
         False => "Ninety four"

valToString : (x : Bool) -> stringOrInt x -> String
valToString x val = case x of
                         True => cast val
                         False => val

allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
