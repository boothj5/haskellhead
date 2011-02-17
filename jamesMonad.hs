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



