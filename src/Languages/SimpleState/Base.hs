{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Languages.SimpleState.Base where

import qualified Data.Map as Map
import Modular.Base
import Prelude hiding (lookup)
import qualified Prelude

-- Value maps
class ValueMap map k v where
  lookup :: k -> map k v -> Maybe v
  insert :: k -> v -> map k v -> map k v

newtype AssocList k v = AssocList { assocs :: [(k, v)] } deriving Show
instance Eq k => ValueMap AssocList k v where
  lookup k   = Prelude.lookup k . assocs
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
data VarExpr   a = Var String | Assign String a

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
  f `fmap` Val n   = Val n
  f `fmap` Add x y = Add (f x) (f y)

instance Functor VarExpr where
  f `fmap` Var v      = Var v
  f `fmap` Assign v x = Assign v $ f x
