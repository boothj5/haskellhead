import Control.Applicative

data JamesContext a = James a
    deriving (Show)

instance Functor JamesContext where
    fmap f (James x) = James (f x)

instance Applicative JamesContext where
    pure x = James x
    (James f) <*> (James x) = James (f x)

instance Monad JamesContext where
    return x = James x
    James x >>= f = f x

myFunction :: (Num n) => n -> n -> n -> n
myFunction x y z = x * y + z


myFuncOnJames :: (Num n) => JamesContext n -> JamesContext n -> JamesContext n -> JamesContext n
myFuncOnJames j1 j2 j3 = do
    x <- j1
    y <- j2
    z <- j3
    James $ myFunction x y z

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
        


