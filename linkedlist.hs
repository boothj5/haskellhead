import Control.Applicative
  
data LinkedList a = EmptyList | Node a (LinkedList a)
    deriving (Show)

instance Functor LinkedList where
    fmap f EmptyList = EmptyList
    fmap f (Node item (rest)) = Node (f item) (fmap f (rest))

instance Applicative LinkedList where
   pure n = Node n (pure n)
   EmptyList <*> _ = EmptyList
   _ <*> EmptyList = EmptyList
   (Node f (r1)) <*> (Node n (r2)) = Node (f n) (r1 <*> r2)

listInsert :: (Ord a) => a -> LinkedList a -> LinkedList a
listInsert item EmptyList       = Node item (EmptyList)
listInsert item (Node elem (rest)) 
    | item <= elem  = Node item (Node elem (rest))
    | item > elem   = Node elem (listInsert item rest)

listGet :: (Integral n) => n -> LinkedList a -> Maybe a
listGet index EmptyList  = Nothing 
listGet 0 (Node elem _) = Just elem
listGet index (Node elem (rest)) = (listGet (index - 1) rest)

ifDivBy3Add2 :: (Integral t) => t -> t
ifDivBy3Add2 n | (rem n 3 /= 0) = n
               | otherwise      = n + 2


main = do
    let myList = Node 1 (Node 2 (Node 3 (Node 4 (Node 5 (Node 6 (Node 7 (Node 8 (Node 9 (Node 10 EmptyList)))))))))
    putStrLn "myList:"
    putStrLn $ show myList
    putStrLn ""
    putStrLn "Elements 0, 5 and 10"
    putStrLn $ show $ listGet 0 myList
    putStrLn $ show $ listGet 5 myList
    putStrLn $ show $ listGet 10 myList
    putStrLn ""
 
    let newList = fmap (\value -> value * 100) myList
    putStrLn "Multiply each item by 100 using fmap"
    putStrLn $ show newList
    putStrLn ""

    let biggerThan2 = fmap (>2) myList
    putStrLn "Test each bigger than 2 using fmap"
    putStrLn $ show biggerThan2
    putStrLn ""
 
    let anotherList = fmap ifDivBy3Add2 myList
    putStrLn "If divisible by 3 add 2"
    putStrLn $ show anotherList
    putStrLn ""
 
    let functionList = fmap (*) myList
        mappedFunctionList = fmap (\value -> value 2) functionList
    putStrLn "fmap a function (which takes a function and passes it 2) to a list"
    putStrLn "containing items of the function (item *)"     
    putStrLn $ show mappedFunctionList
    putStrLn ""
 
    let applicativeMappedList = functionList <*> myList
    putStrLn "Apply the list with item = (item *) to the myList, essentially squaring the list"
    putStrLn $ show applicativeMappedList
    putStrLn ""
 
    let addedLists = pure (+) <*> myList <*> applicativeMappedList
    putStrLn "Add myList to the previous list"
    putStrLn $ show addedLists
    putStrLn ""
 
    let alternativeAddedLists = (+) <$> myList <*> applicativeMappedList
    putStrLn "Add with '(+) <$> list1 <*> list2' instead of 'pure (+) <*> list1 <*> list2'"
    putStrLn $ show alternativeAddedLists
    putStrLn ""
 
    let threeAddedLists = (+) <$> ((+) <$> myList <*> applicativeMappedList) <*> newList
    putStrLn "Add the *100 list too with  '(+) <$> ((+) <$> list1 <*> list2) <*> list3'"
    putStrLn $ show threeAddedLists

