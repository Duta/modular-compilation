{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Modular.Evaluator where

import Modular.Base

class Eval f m v where
  evalAlgebra :: Algebra f (m v)

instance (Eval f m v, Eval g m v) => Eval (f :+: g) m v where
  evalAlgebra = either evalAlgebra evalAlgebra . runLift

eval :: (Functor f, Eval f m v) => Fix f -> m v
eval = cata evalAlgebra
