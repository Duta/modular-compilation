{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Languages.SimpleState where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.State (StateT(runStateT), get, lift, modify)
import Modular.Evaluator
import qualified Data.Map as Map
import Prelude hiding (lookup)
import qualified Prelude

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
