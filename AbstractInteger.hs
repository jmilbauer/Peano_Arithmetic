module AbstractInteger (
       AbstractInteger(Zero, S),
successor,
negator,
absolute,
predecessor,
add,
difference,
multiply,
divide,
modulo,
toAbstract,
fromAbstract,
solveRPN,
  one,
  two,
  three,
  four,
  five,
  six,
  seven,
  eight,
  nine,
  ten,
  negative_one,
  negative_two,
  negative_three,
  negative_four,
  negative_five,
  negative_six,
  negative_seven,
  negative_eight,
  negative_nine,
  negative_ten) where

-- // DATA TYPE DEFINITION -------

data AbstractInteger = Zero | S AbstractInteger | P AbstractInteger
  deriving (Show)

successor :: AbstractInteger -> AbstractInteger
successor = S

predecessor :: AbstractInteger -> AbstractInteger
predecessor = P

--This function exists because I'm worried about the instantiation of Eq and Ord
clean :: AbstractInteger -> AbstractInteger
clean Zero          = Zero
clean (P Zero)      = P Zero
clean (S Zero)      = S Zero
clean (S (P x))     = clean x
clean (P (S x))     = clean x
--Build this in terms of a building up of an abstract
clean x             = toAbstract $ fromAbstract x

-- // COMPARISON INSTANTIATION -------

instance Eq AbstractInteger where
    P (S x) == y        = x == y
    S (P x) == y        = x == y
    x == (S (P y))      = x == y
    x == (P (S y))      = x == y

    (P x) == (P y)      = x == y
    (S x) == (S y)      = x == y

    P x == S y          = P (P x) == y
    S x == P y          = S (S x) == y

    Zero == Zero        = True

    S _ == Zero         = False
    P _ == Zero         = False
    Zero == S _         = False
    Zero == P _         = False

instance Ord AbstractInteger where
    compare (P (S x)) y     = compare x y
    compare (S (P x)) y     = compare x y
    compare x (S (P y))     = compare x y
    compare x (P (S y))     = compare x y

    compare (P x) (P y)     = compare x y
    compare (S x) (S y)     = compare x y

    compare Zero Zero       = EQ
    compare Zero (P _)      = GT
    compare (P _) Zero      = LT
    compare Zero (S _)      = LT
    compare (S _) Zero      = GT

-- // SIGN OPERATIONS -------

negator :: AbstractInteger -> AbstractInteger
negator Zero        = Zero
negator (S y)       = clean $ P (negator y)
negator (P y)       = clean $ S (negator y)

absolute :: AbstractInteger -> AbstractInteger
absolute x
    | x >= zero     = clean $ x
    | otherwise     = clean $ negator x


-- // SIMPLE OPERATIONS -------

add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add x Zero          = clean $ x
add x (S y)         = clean $ S $ add x y
add x (P y)         = clean $ P $ add x y

difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference Zero x   = clean $ negator x
difference x Zero   = clean $ x
difference x (S y)  = clean $ P $ difference x y
difference x (P y)  = clean $ S $ difference x y

multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger
multiply x Zero     = Zero
multiply x (S y)    = clean $ add (multiply x y) x
multiply x (P y)    = clean $ difference (multiply x y) x


-- // DIVISION OPERATIONS -------

-- a / b = q | qb = a - k where q is an Integer and k is the minimum positive value.
divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide x y
    | y == Zero     = error "divide: cannot divide by zero"
    | y < Zero      = clean $ negator $ divide x $ absolute y
    | x < Zero      = clean $ P $ flip divide y $ add x y
    | x >= y        = clean $ S $ flip divide y $ difference x y
    | otherwise     = Zero

-- a mod b = q | qb = a - k where q is an Integer and k is the minimum positive value.
modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo x y
    | y == Zero     = error "modulo: cannot divide by zero"
    | x == Zero     = Zero
--This is how WolframAlpha does it:
--  | y < zero      = clean $ negator $ modulo (negator x) (absolute y)
--but this version is better algebra
    | y < Zero      = clean $ modulo x (absolute y)
    | x < Zero      = clean $ modulo (add x y) y
    | otherwise     = clean $ difference x (multiply y (divide x y))


-- // CONVERSION OPERATIONS -------

toAbstract :: (Integral a) => a -> AbstractInteger
toAbstract 0 = Zero
toAbstract x
    | x > 0 = S $ toAbstract $ x - 1
    | x < 0 = P $ toAbstract $ x + 1

fromAbstract :: (Integral a) => AbstractInteger -> a
fromAbstract Zero = 0
fromAbstract (S x) = (fromAbstract x) + 1
fromAbstract (P x) = (fromAbstract x) - 1

solveRPN :: [String] -> Integer
solveRPN  = fromAbstract . head . foldl foldingFunction []
  where
    foldingFunction (x:y:ys) "*" = (multiply x y):ys
    foldingFunction (x:y:ys) "+" = (add x y):ys
    foldingFunction (x:xs) "abs" = (absolute x):xs
    foldingFunction (x:y:ys) "-" = (difference y x):ys
    foldingFunction (x:y:ys) "/" = (divide y x):ys
    foldingFunction (x:y:ys) "%" = (modulo y x):ys
    foldingFunction xs numberString =
      (toAbstract $ (read (numberString :: String) :: Integer)):xs


zero = Zero

one = successor Zero
two = successor one
three = successor two
four = successor three
five = successor four
six = successor five
seven = successor six
eight = successor seven
nine = successor eight
ten = successor nine
infinity = successor infinity

negative_one = predecessor Zero
negative_two = predecessor negative_one
negative_three = predecessor negative_two
negative_four = predecessor negative_three
negative_five = predecessor negative_four
negative_six = predecessor negative_five
negative_seven = predecessor negative_six
negative_eight = predecessor negative_seven
negative_nine = predecessor negative_eight
negative_ten = predecessor negative_nine
negative_infinity = predecessor negative_infinity
