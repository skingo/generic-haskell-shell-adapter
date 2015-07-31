{-# LANGUAGE TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module Base ((:+:)(..)
            , (:≺:)
            , inj
            ) where

-- the coproduct
-- needs TypeOperators for infix type constructor
infixr :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl a) = Inl (fmap f a)
    fmap f (Inr b) = Inr (fmap f b)

-- the a type parameter needs FlexibleContexts
instance (Show (f a), Show (g a)) => Show ((f :+: g) a) where
    show (Inl f) = show f
    show (Inr g) = show g

-- type relation specifying subtype relation (needs MultiParamTypeClasses)
-- ≺ has unicode 227A
class (Functor sub, Functor sup) => (sub :≺: sup) where
    inj :: sub a -> sup a

-- needs FlexibleInstances for infix Relation and OverlappingInstances
instance Functor f => f :≺: f where
    inj = id

instance (Functor f, Functor g) => f :≺: (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f :≺: g) => f :≺: (h :+: g) where
    inj = Inr . inj
