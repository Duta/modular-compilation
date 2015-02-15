{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module ModularEvaluator where

import Prelude hiding (lookup)
import qualified Prelude
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.State (StateT, get, modify, lift, runStateT)
import qualified Data.Map as Map

-- Value maps
class ValueMap map k v where
  lookup :: k -> map k v -> Maybe v
  insert :: k -> v -> map k v -> map k v

newtype AssocList k v = AssocList { assocs :: [(k, v)] } deriving Show
instance Eq k => ValueMap AssocList k v where
  lookup k = Prelude.lookup k . assocs
  insert k v = AssocList . insert' . assocs
   where
    insert' [] = [(k, v)]
    insert' ((k', v'):t)
      | k == k' = (k, v):t
      | otherwise = (k', v'):insert' t

instance Ord k => ValueMap Map.Map k v where
  lookup = Map.lookup
  insert = Map.insert

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

eval :: (Functor f, Eval f m v) => Fix f -> m v
eval = cata evalAlgebra

instance (Eval f m v, Eval g m v) => Eval (f :+: g) m v where
  evalAlgebra = either evalAlgebra evalAlgebra . runLift

-- Expression types
type Expr = Fix (ArithExpr :+: VarExpr)
data ArithExpr a = Val Int | Add a a
data VarExpr a = Var String | Assign String a

-- Smart constructors
val :: (ArithExpr :<: f) => Int -> Fix f
val = inject . Val

add :: (ArithExpr :<: f) => Fix f -> Fix f -> Fix f
add x y = inject $ Add x y

var :: (VarExpr :<: f) => String -> Fix f
var = inject . Var

assign :: (VarExpr :<: f) => String -> Fix f -> Fix f
assign v x = inject $ Assign v x

-- Functor instances
instance Functor ArithExpr where
  f `fmap` Val n = Val n
  f `fmap` Add x y = Add (f x) (f y)

instance Functor VarExpr where
  f `fmap` Var v = Var v
  f `fmap` Assign v x = Assign v $ f x

-- Eval instances
instance Applicative m => Eval ArithExpr m Int where
  evalAlgebra (Val x) = pure x
  evalAlgebra (Add x y) = (+) <$> x <*> y

instance ValueMap map String v => Eval VarExpr (StateT (map String v) Maybe) v where
  evalAlgebra (Var v) = get >>= lift . lookup v
  evalAlgebra (Assign v x) = do
    x' <- x
    modify $ insert v x'
    return x'

-- :D
evalExpr :: ValueMap map String Int => Expr -> map String Int -> Maybe (Int, map String Int)
evalExpr = runStateT . eval
