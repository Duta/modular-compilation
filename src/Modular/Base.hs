{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Modular.Base where

-- Bifunctors
class Bifunctor p where
  bimap :: (a -> a') -> (b -> b') -> p a b -> p a' b'
  bimap f g = first f . second g
  first :: (a -> a') -> p a b -> p a' b
  first f = bimap f id
  second :: (b -> b') -> p a b -> p a b'
  second g = bimap id g

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

instance Bifunctor (,) where
  bimap f g (x, y) = (f x, g y)

-- Functor products/coproducts
newtype Lift p f g a = Lift { runLift :: p (f a) (g a) }
type (:*:) = Lift (,)
type (:+:) = Lift Either
inl = Lift . Left
inr = Lift . Right

instance (Bifunctor p, Functor f, Functor g) => Functor (Lift p f g) where
  fmap f = Lift . bimap (fmap f) (fmap f) . runLift

-- Functor subtyping
class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = inl . inj

instance (Functor f, Functor g) => f :<: (g :+: f) where
  inj = inr

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj

-- Fixed-points
newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . out

-- F-algebras
type Algebra f a = f a -> a
