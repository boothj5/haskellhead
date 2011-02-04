import Data.List  
import Control.Applicative
  
-- -----------------------------------------
-- different approaches to calling functions
-- -----------------------------------------

-- Using the usual pattern matching and standard (left) associativity
numUniques1 :: (Eq a) => [a] -> Int  
numUniques1 xs = length (nub xs)

-- Using function application (right associative)
numUniques2 :: (Eq a) => [a] -> Int  
numUniques2 xs = length $ nub xs

-- Using function composition
numUniques3 :: (Eq a) => [a] -> Int  
numUniques3 = length . nub

-- Using a lambda function
numUniques4 :: (Eq a) => [a] -> Int  
numUniques4 = \xs -> length (nub xs)

-- Using a lambda function that uses function application
numUniques5 :: (Eq a) => [a] -> Int  
numUniques5 = \xs -> length $ nub xs

-- Combining function composition and function application
-- The associaticivity below is
--      nub is first combined with length (left) 
--          the result is a function that expects as list
--      xs is passed to this new function composition by applying xs (right)
numUniques6 :: (Eq a) => [a] -> Int  
numUniques6 = \xs -> length . nub $ xs



-- Foobar problem with guards, recursion and pattern matching
printFooBar x | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "foobar"
			  | (x `mod` 3 == 0) 					 = "foo"
			  | (x `mod` 5 == 0) 					 = "bar"
			  | otherwise							 = show x

fooBar100 []     = []
fooBar100 (x:[]) = printFooBar x : []
fooBar100 (x:xs) = printFooBar x : fooBar100 xs

-- Foobar problem using list comprehension 
fooBarBetter xs = [ if ((x `mod` 3 == 0) && (x `mod` 5 == 0)) then "foobar" 
					else if (x `mod` 3 == 0) then "foo"
					else if (x `mod` 5 == 0) then "bar"
					else show x
					| x <- xs , x <= 100 ]


-- More list comprehension examples
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]  
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  

-- quicksort using let and list comprehension
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- --------------------------------------------------------------------
-- Different appraoches to finding a key in an association list [(k,v)]
-- Just for illustration, this is all in Data.Map
-- --------------------------------------------------------------------

phonebook = [("James", "07747777468"), ("Home", "01473749749")]

-- compose snd (second in tuple), head (first in list) and filter (using a lambda
-- predicate to return those that match)
-- This composed function will take a list so pass it using function application
-- This implementation will result in exception if key not present (cannot call head on empty list)
findKey1 :: (Eq k) => k -> [(k,v)] -> v
findKey1 key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- Exactly the same, but returns a partial function (which requires a list) instead
findKey2 :: (Eq k) => k -> [(k,v)] -> v
findKey2 key = snd . head . filter (\(k,v) -> key == k)

-- Use Maybe data type so we can return nothing, or one element
findKey3 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey3 key [] = Nothing  
findKey3 key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey3 key xs

-- Use a fold such a common pattern
-- Note, we're not passing the list, so again we return a partial function that will
-- take a list as a paramater
findKey4 :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey4 key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing

-- --------------------------
-- Higher order functions
-- Type classes, functors etc
-- --------------------------

myAdd2 :: (Num a) => a -> a
myAdd2 = (+) 2

myAdd2AndShow :: (Num a) => a -> String
myAdd2AndShow = show . myAdd2

-- Map is an fmap over lists
-- fmap :: (a -> b) -> f a -> f b
--  takes 
--      function of type a to type b
--      type contrucur that takes type a as parameter (think Maybe)
--      returns type contructor that takes type b as parameter
-- map :: (a -> b) -> [a] -> [b]
--  takes
--      function of type a to type b
--      type contructor of [] a
--      return types constructor of [] b

-- map myAdd2 [1,2,3,4]
-- map show [1,2,3,4]

-- Types that can act like boxes can be instances of Functor
-- Lists, Maybe, LinkedList (below)
--
-- fmap myAdd2 $ Just 3
-- fmap show $ Just 3

-- LinkedList of any type to learn about functors
data LinkedList a = EmptyList | Node a (LinkedList a)
    deriving (Show)

-- Create my instance of functor
instance Functor LinkedList where
    fmap f EmptyList = EmptyList
    fmap f (Node item (rest)) = Node (f item) (fmap f (rest))

instance Applicative LinkedList where
   pure n = Node n (pure n)
   EmptyList <*> _ = EmptyList
   _ <*> EmptyList = EmptyList
   (Node f (r1)) <*> (Node n (r2)) = Node (f n) (r1 <*> r2)


-- insert an ordered elemet
listInsert :: (Ord a) => a -> LinkedList a -> LinkedList a
listInsert item EmptyList       = Node item (EmptyList)
listInsert item (Node elem (rest)) 
    | item <= elem  = Node item (Node elem (rest))
    | item > elem   = Node elem (listInsert item rest)

-- get an element by index (starting at 0)
listGet :: (Integral n) => n -> LinkedList a -> Maybe a
listGet index EmptyList  = Nothing 
listGet 0 (Node elem _) = Just elem
listGet index (Node elem (rest)) = (listGet (index - 1) rest)

-- If the integral is divisible by 3, add 2, otherwise return the integral
ifDivBy3Add2 :: (Integral t) => t -> t
ifDivBy3Add2 n | (rem n 3 /= 0) = n
               | otherwise      = n + 2


-- LinkedLists to test out the functors
myList = Node 1 (Node 2 (Node 3 (Node 4 (Node 5 (Node 6 (Node 7 (Node 8 (Node 9 (Node 10 EmptyList)))))))))

-- simple function map, multiply elemets by 100
-- the following:
--      fmap (*100) myList
-- is tha same, but this helps to show how the mapped function list works later
newList = fmap (\value -> value * 100) myList
-- newCharList = fmap (\value -> value * 100) myCharList

-- Shows how the list of one type can be mapped to a completely different type
-- Integral -> Bool
biggerThan2 = fmap (>2) myList

-- map my own function
anotherList = fmap ifDivBy3Add2 myList

-- create a list of partial functions
-- item 0 = 1 * 
-- item 1 = 2 *
-- etc
functionList = fmap (*) myList

-- Madness, for each element in the list, apply the function in tha map
-- Value gets replace with a partial function which is then applied to 2
-- So:
-- Item 0 = 1 *
-- NewItem 0 = (\1 * -> 1 * 2)   <- Substitute in the lambda
-- NewItem 0 = 2
-- Item 1 = 2 *
-- NewItem 1 = (\2 * -> 2 * 2)   <- Substitute in the lambda
-- NewItem 1 = 4
-- etc
mappedFunctionList = fmap (\value -> value 2) functionList

-- How about applying one list of partial functions, to another list of values??
-- Use applicative functor, see instance above
applicativeMappedList = functionList <*> myList

-- add two LinkedLists together
addedLists = pure (+) <*> myList <*> applicativeMappedList

-- alternative added lists
alternativeAddedLists = (+) <$> myList <*> applicativeMappedList

-- Add three lists
threeAddedLists = (+) <$> ((+) <$> myList <*> applicativeMappedList) <*> newList

-- Main IO action for testing
main = do

    
    putStrLn "myList:"
    putStrLn $ show myList
    putStrLn ""
    putStrLn "Multiply each item by 100 using fmap"
    putStrLn $ show newList
    putStrLn ""
    putStrLn "Test each bigger than 2 using fmap"
    putStrLn $ show biggerThan2
    putStrLn ""
    putStrLn "If divisible by 3 add 2"
    putStrLn $ show anotherList
    putStrLn ""
    putStrLn "Elements 0, 5 and 10"
    putStrLn $ show $ listGet 0 myList
    putStrLn $ show $ listGet 5 myList
    putStrLn $ show $ listGet 10 myList
    putStrLn ""
    putStrLn "fmap a function (which takes a function and passes it 2) to a list"
    putStrLn "containing items of the function (item *)"     
    putStrLn $ show mappedFunctionList
    putStrLn ""
    putStrLn "Apply the list with item = (item *) to the myList, essentially squaring the list"
    putStrLn $ show applicativeMappedList
    putStrLn ""
    putStrLn "Add myList to the previous list"
    putStrLn $ show addedLists
    putStrLn ""
    putStrLn "Add with '(+) <$> list1 <*> list2' instead of 'pure (+) <*> list1 <*> list2'"
    putStrLn $ show alternativeAddedLists
    putStrLn ""
    putStrLn "Add the *100 list too with  '(+) <$> ((+) <$> list1 <*> list2) <*> list3'"
    putStrLn $ show threeAddedLists


--    putStrLn $ show $ myFunction 10
--    putStrLn $ show appliedList
