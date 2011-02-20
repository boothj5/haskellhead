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

multThree :: (Num n) => SomethingContext n -> SomethingContext n -> SomethingContext n -> SomethingContext n
multThree s1 s2 s3 = do 
    x <- s1 
    y <- s2  
    z <- s3  
    return (x*y*z)

multThreeIgnoreSecond :: (Num n) => SomethingContext n -> SomethingContext n -> SomethingContext n -> SomethingContext n
multThreeIgnoreSecond s1 s2 s3 = do 
    x <- s1 
    s2  
    z <- s3  
    return (x*z)



main1 = 
    getLine >>= (\first -> 
    putStrLn ("first thing was: " ++ first) >>= (\_ -> 
    getLine >>= (\second -> 
    putStrLn ("second thing was: " ++ second) >>= (\_ ->
    fmap read getLine >>= (\number ->
    if number > 10 
        then putStrLn ("Bigger") 
        else putStrLn ("Smaller"))))))

main2 = 
    getLine >>= (\first -> 
    putStrLn ("first thing was: " ++ first) >> ( 
    getLine >>= (\second -> 
    putStrLn ("second thing was: " ++ second) >> (
    fmap read getLine >>= (\number ->
    if number > 10 
        then putStrLn ("Bigger") 
        else putStrLn ("Smaller"))))))

main3 = 
    getLine >>= \first -> 
    putStrLn ("first thing was: " ++ first) >>  
    getLine >>= \second -> 
    putStrLn ("you said: " ++ second) >> 
    fmap read getLine >>= \number ->
    if number > 10 
        then putStrLn ("Bigger") 
        else putStrLn ("Smaller")

main4 = do
    first <- getLine
    putStrLn ("first thing was: " ++ first)
    second <- getLine
    putStrLn ("you said: " ++ second)
    number <- fmap read getLine
    if number > 10
        then putStrLn ("Bigger")
        else putStrLn ("Smaller")
        


