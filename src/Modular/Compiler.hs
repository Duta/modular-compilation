{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Modular.Compiler where

import Modular.Base

class Compile f code where
  compileAlgebra :: Algebra f (code -> code)

instance (Compile f code, Compile g code) => Compile (f :+: g) code where
  compileAlgebra = either compileAlgebra compileAlgebra . runLift

compile :: (Functor f, Compile f code) => Fix f -> code -> code
compile = cata compileAlgebra
