{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Modular.Evaluator where

import Control.Applicative (Applicative((<*>)), (<$>))

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

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = inl

instance (Functor f, Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
  inj = inr . inj

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj

-- Fixed-points
newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . out

-- F-algebras
type Algebra f a = f a -> a

-- Eval
class Eval f m v where
  evalAlgebra :: Algebra f (m v)

instance (Eval f m v, Eval g m v) => Eval (f :+: g) m v where
  evalAlgebra = either evalAlgebra evalAlgebra . runLift

eval :: (Functor f, Eval f m v) => Fix f -> m v
eval = cata evalAlgebra
