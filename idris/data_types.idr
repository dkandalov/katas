data DumbNumber = Naught | One | Two | Three
data Natural = Zero | After Natural
data MyList a = Blank | (::) a (MyList a)

data MMaybe a = Nothing | Just a

first : MyList a -> MMaybe a
first Blank = Nothing
first (x :: xs) = Just x

filterGreaterThan : Integer -> List Integer -> List Integer
filterGreaterThan n list = filter (\x => x > n) list

everyOtherElement : List a -> List a
everyOtherElement [] = []
everyOtherElement (x :: xs) = x :: (everyOtherElement (drop 1 xs))

reverseList : List a -> List a
reverseList [] = []
reverseList (x :: xs) = (reverseList xs) ++ [x]

data Tree a = Leaf a | Node (Tree a) (Tree a)