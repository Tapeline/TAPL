module Lambda.LamResults where

data Result err ok
  = Err err
  | Ok ok
  deriving (Show, Eq)

instance Functor (Result err) where
  fmap _ (Err e) = Err e
  fmap f (Ok x) = Ok (f x)

instance Applicative (Result err) where
    pure = Ok
    Err err <*> _ = Err err
    Ok f <*> something = fmap f something

instance Monad (Result err) where
    Err err >>= _ = Err err
    Ok x >>= f = f x
