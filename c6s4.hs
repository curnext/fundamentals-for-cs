data RegExp
  = Epsilon
  | EmptySet
  | Literal Char
  | Concat RegExp RegExp
  | Star RegExp
  | Union RegExp RegExp
  deriving (Show, Eq)

derivation :: RegExp -> [Char] -> RegExp
derivation = foldl derivation'

derivation' :: RegExp -> Char -> RegExp
derivation' EmptySet a = EmptySet
derivation' Epsilon a = EmptySet
derivation' (Literal b) a = if a == b then Epsilon else EmptySet
derivation' (Union e1 e2) a = Union (derivation' e1 a) (derivation' e2 a)
derivation' (Concat e1 e2) a =
  if nullable e1
    then Union (Concat (derivation' e1 a) e2) (derivation' e2 a)
    else Concat (derivation' e1 a) e2
derivation' (Star e) a = Concat (derivation' e a) (Star e)

nullable :: RegExp -> Bool
nullable EmptySet = False
nullable Epsilon = True
nullable (Literal c) = False
nullable (Union e1 e2) = nullable e1 || nullable e2
nullable (Concat e1 e2) = nullable e1 && nullable e2
nullable (Star e) = True

belong :: RegExp -> [Char] -> Bool
belong e w = nullable $ derivation e w
