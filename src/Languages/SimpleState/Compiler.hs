{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Languages.SimpleState.Compiler where

import Languages.SimpleState.Base
import Modular.Base
import Modular.Compiler

-- Instruction types
type Bytecode = Fix (ARITH :+: VAR :+: EMPTY)
data ARITH a = CONST Int a | ADD a
data VAR   a = LOAD String a | STORE String a
data EMPTY a = NULL

-- Smart constructors
c_const :: (ARITH :<: f) => Int -> Fix f -> Fix f
c_const n = inject . CONST n

c_add :: (ARITH :<: f) => Fix f -> Fix f
c_add = inject . ADD

c_load :: (VAR :<: f) => String -> Fix f -> Fix f
c_load v = inject . LOAD v

c_store :: (VAR :<: f) => String -> Fix f -> Fix f
c_store v = inject . STORE v

c_null :: (EMPTY :<: f) => Fix f
c_null = inject NULL

-- Functor instances
instance Functor ARITH where
  f `fmap` CONST n c = CONST n $ f c
  f `fmap` ADD     c = ADD     $ f c
instance Functor VAR where
  f `fmap` LOAD  v c = LOAD  v $ f c
  f `fmap` STORE v c = STORE v $ f c
instance Functor EMPTY where
  f `fmap` NULL = NULL

-- Compile instances
instance Compile ArithExpr Bytecode where
  compileAlgebra (Val n)   = c_const n
  compileAlgebra (Add x y) = x . y . c_add

instance Compile VarExpr Bytecode where
  compileAlgebra (Var v)      = c_load v
  compileAlgebra (Assign v x) = x . c_store v

-- The compiler
compileExpr :: Expr -> Bytecode
compileExpr expr = compile expr c_null
