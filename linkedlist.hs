import Control.Applicative
import Data.Monoid
  
data LinkedList a = EmptyList | Node a (LinkedList a)
    deriving (Show)

newtype ConcList a = ConcList { getConcList :: LinkedList a }
    deriving (Show)

instance Functor LinkedList where
    fmap f EmptyList          = EmptyList
    fmap f (Node item (rest)) = Node (f item) (fmap f (rest))

instance Applicative LinkedList where
    pure n = Node n (pure n)
    EmptyList <*> _ = EmptyList
    _ <*> EmptyList = EmptyList
    (Node f (r1)) <*> (Node n (r2)) = Node (f n) (r1 <*> r2)
--    (Node n (r1)) <*> (Node f (r2)) = Node (f n) (r1 <*> r2)

instance Monoid (ConcList a) where
    mempty = ConcList EmptyList
    mappend l (ConcList EmptyList) = l
    mappend (ConcList EmptyList) l = l
    mappend (ConcList (Node n (EmptyList))) l2 = ConcList (Node n (getConcList l2))
    mappend (ConcList (Node n r1)) l2 = ConcList (Node n (getConcList (((ConcList r1) `mappend` l2))))

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
    putStrLn "myList"
    putStrLn $ show myList
    putStrLn ""
    putStrLn "Elements 0, 5 and 10"
    putStrLn $ show $ listGet 0 myList
    putStrLn $ show $ listGet 5 myList
    putStrLn $ show $ listGet 10 myList
    putStrLn ""
 
    let newList = fmap (\value -> value * 100) myList
    putStrLn "newList = fmap (\\value -> value * 100) myList"
    putStrLn $ show newList
    putStrLn ""

    let biggerThan2 = fmap (>2) myList
    putStrLn "biggerThan2 = fmap (>2) myList"
    putStrLn $ show biggerThan2
    putStrLn ""
 
    let anotherList = fmap ifDivBy3Add2 myList
    putStrLn "anotherList = fmap ifDivBy3Add2 myList"
    putStrLn $ show anotherList
    putStrLn ""
 
    let functionList = fmap (*) myList
        mappedFunctionList = fmap (\value -> value 2) functionList
    putStrLn "functionList = fmap (*) myList"
    putStrLn "mappedFunctionList = fmap (\\value -> value 2) functionList"
    putStrLn $ show mappedFunctionList
    putStrLn ""
 
    let applicativeMappedList = functionList <*> myList
    putStrLn "applicativeMappedList = functionList <*> myList"
    putStrLn $ show applicativeMappedList
    putStrLn ""
 
    let addedLists = pure (+) <*> myList <*> applicativeMappedList
    putStrLn "addedLists = pure (+) <*> myList <*> applicativeMappedList"
    putStrLn $ show addedLists
    putStrLn ""
 
    let alternativeAddedLists = (+) <$> myList <*> applicativeMappedList
    putStrLn "alternativeAddedLists = (+) <$> myList <*> applicativeMappedList"
    putStrLn $ show alternativeAddedLists
    putStrLn ""
 
    let threeAddedLists = (+) <$> ((+) <$> myList <*> applicativeMappedList) <*> newList
    putStrLn "threeAddedLists = (+) <$> ((+) <$> myList <*> applicativeMappedList) <*> newList"
    putStrLn $ show threeAddedLists
    putStrLn ""
    
    let orderList = Node "First:" (Node "Second:" (Node "Third:" (Node "Fourth:" EmptyList)))
        someStrList = Node "James" (Node "Steve" (Node "Dave" (Node "Mike" EmptyList)))
        concatList = (++) <$> orderList <*> someStrList
    putStrLn "orderList:"
    putStrLn $ show orderList
    putStrLn "someStrList:"
    putStrLn $ show someStrList
    putStrLn "concatList = (++) <$> orderList <*> someStrList"
    putStrLn $ show concatList
    putStrLn ""
 
    let firstList = Node 2 (Node 3 EmptyList)
        secondList = Node 10 (Node 12 EmptyList)
        concatList = getConcList $ ConcList firstList `mappend` ConcList secondList
        concatThreeList = getConcList $ ConcList firstList `mappend` ConcList secondList `mappend` ConcList firstList

    putStrLn "firstList:"
    putStrLn $ show firstList
    putStrLn "secondList:"
    putStrLn $ show secondList
    putStrLn "getConcList $ ConcList firstList `mappend` ConcList secondList"
    putStrLn $ show concatList
    putStrLn "getConcList $ ConcList firstList 'mappend' ConcList secondList `mappend` ConcList firstList"
    putStrLn $ show concatThreeList

