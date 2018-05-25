data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just eq) => Just (sameS k j eq)


exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 (Just (Same len)) => Just input


checkEqN : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqN Z Z = Just Refl
checkEqN Z (S k) = Nothing
checkEqN (S k) Z = Nothing
checkEqN (S k) (S j) = case checkEqN k j of
                            Nothing => Nothing
                            (Just prf) => Just (cong prf)

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons {xs = []} {ys = []} prf = cong prf
same_cons {xs = []} {ys = (x :: xs)} prf = cong prf
same_cons {xs = (x :: xs)} {ys = []} prf = cong prf
same_cons {xs = (x :: xss)} {ys = (y :: yss)} prf = cong prf
