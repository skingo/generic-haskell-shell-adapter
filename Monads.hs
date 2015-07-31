{-# LANGUAGE TypeOperators #-}

module Monads (Term(..)
              , foldTerm
              , inject
              -- should maybe be left out...:
              , IdMonad
              , MaybeMonad
              , One(..)
              , ErrorMonad(..)
              , Const(..)
              ) where

import Base

data Term f a = Pure a | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
    fmap f (Pure a)   = Pure $ f a
    fmap f (Impure t) = Impure $ fmap (fmap f) t

instance Functor f => Monad (Term f) where
    return            = Pure
    (Pure x)   >>= f  = f x
    (Impure t) >>= f  = Impure $ fmap (>>= f) t

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure _   (Pure x)   = pure x
foldTerm pure imp (Impure t) = imp $ fmap (foldTerm pure imp) t

inject :: (g :≺: f) => g (Term f a) -> Term f a
inject = Impure . inj

data Zero a
data One a = One
data Const e a = Const e

type IdMonad = Term Zero
type MaybeMonad = Term One
data ErrorMonad e a = E (Term (Const e) a)
