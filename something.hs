import Control.Applicative
import Data.Monoid

data SomethingContext a = Something a
    deriving (Show)

instance Functor SomethingContext where
    fmap f (Something x) = Something (f x)

instance Applicative SomethingContext where
    pure x = Something x
    (Something f) <*> (Something x) = Something (f x)

instance Num a => Monoid (SomethingContext a) where
    mempty = Something 0
    mappend j1 j2 = addSomething j1 j2

instance Monad SomethingContext where
    return x = Something x
    Something x >>= f = f x

addSomething :: (Num a) => SomethingContext a -> SomethingContext a -> SomethingContext a
addSomething (Something x) (Something y) = Something (x + y)

myFunction :: (Num n) => n -> n -> n -> n
myFunction x y z = x * y + z

myFuncOnSomething :: (Num n) => SomethingContext n -> SomethingContext n -> SomethingContext n -> SomethingContext n
myFuncOnSomething j1 j2 j3 = do
    x <- j1
    y <- j2
    z <- j3
    Something $ myFunction x y z

main1 = 
    getLine >>= (\first -> 
    putStrLn ("first thing was: " ++ first) >>= (\_ -> 
    getLine >>= (\line -> 
    putStrLn ("you said: " ++ line) >>= (\_ ->
    fmap read getLine >>= (\number ->
    if number > 10 then putStrLn ("Bigger") else putStrLn ("Smaller"))))))

main2 = 
    getLine >>= (\first -> 
    putStrLn ("first thing was: " ++ first) >> ( 
    getLine >>= (\line -> 
    putStrLn ("you said: " ++ line) >> (
    fmap read getLine >>= (\number ->
    if number > 10 then putStrLn ("Bigger") else putStrLn ("Smaller"))))))

main3 = 
    getLine >>= \first -> 
    putStrLn ("first thing was: " ++ first) >>  
    getLine >>= \line -> 
    putStrLn ("you said: " ++ line) >> 
    fmap read getLine >>= \number ->
    if number > 10 then putStrLn ("Bigger") else putStrLn ("Smaller")

main4 = do
    first <- getLine
    putStrLn ("first thing was: " ++ first)
    line <- getLine
    putStrLn ("you said: " ++ line)
    number <- fmap read getLine
    if number > 10
        then 
            putStrLn ("Bigger")
        else
            putStrLn ("Smaller")
        


