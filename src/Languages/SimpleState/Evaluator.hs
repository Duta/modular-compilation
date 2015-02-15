{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Languages.SimpleState.Evaluator where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.State (StateT(runStateT), get, lift, modify)
import Languages.SimpleState.Base
import Modular.Evaluator
import Prelude hiding (lookup)

-- Eval instances
instance Applicative m => Eval ArithExpr m Int where
  evalAlgebra (Val x)   = pure x
  evalAlgebra (Add x y) = (+) <$> x <*> y

instance ValueMap map String v => Eval VarExpr (StateT (map String v) Maybe) v where
  evalAlgebra (Var v)      = get >>= lift . lookup v
  evalAlgebra (Assign v x) = do
    x' <- x
    modify $ insert v x'
    return x'

-- The evaluator
evalExpr :: ValueMap map String Int => Expr -> map String Int -> Maybe (Int, map String Int)
evalExpr = runStateT . eval
