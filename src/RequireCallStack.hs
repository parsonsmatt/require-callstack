{-# language RankNTypes, MultiParamTypeClasses, DataKinds, ConstraintKinds, ImplicitParams, UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}

-- | This module provides utilities to ensure that you propagate
-- 'HasCallStack' constraints by introducing a class 'RequireCallStack'
-- which can only be discharged using the 'provideCallStack' function.
--
-- Let's say you have a custom prelude for your project, and you want
-- better callstack support. You replace the 'Prelude.error' with a custom
-- variant:
--
-- @
-- error :: RequireCallStack => String -> a
-- error = Prelude.error
-- @
--
-- Now, you will receive a compile-time error at every use site of 'error'
-- in your project. These errors will complain about a missing instance of
-- some weird class that gently suggests to add a 'RequireCallStack'
-- constraint, or use 'provideCallStack' to discharge it. You can add
-- 'RequireCallStack' constraints up the stack, until eventually, you have
-- complete provenance information. Or, if you want to make the work a bit
-- easier, you can use 'provideCallStack' to dismiss the constraint.
--
-- @
-- foo :: `RequireCallStack` => `Int` -> `String`
-- foo = `error` "oh no"
--
-- bar :: `Int` -> `String`
-- bar i = `provideCallStack` `$` foo i
-- @
--
-- … or if you have the @ImplicitParams@ language extension you can also define
-- the @?provideCallStack@ implicit parameter instead of using
-- `provideCallStack`:
--
-- @
-- foo :: `RequireCallStack` => `Int` -> `String`
-- foo = `error` "oh no"
--
-- bar :: `Int` -> `String`
-- bar i = foo i
--   where
--     ?provideCallStack = ()
-- @
--
-- Couple this with @annotated-exception@ library for excellent provenance
-- information on all thrown exceptions.
module RequireCallStack
    ( RequireCallStack
    , RequireCallStackImpl
    , provideCallStack
    , errorRequireCallStack
    ) where

import GHC.Stack (HasCallStack)
import GHC.Classes (IP(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | This constraint is similar to 'HasCallStack' in that it's presence
-- will capture a stack frame for the call site of the function. This helps
-- to preserve callstack provenance, which
--
-- @since 0.1.0.0
type RequireCallStack = (HasCallStack, RequireCallStackImpl)

-- | If you're running into this class, then you need to add
-- 'RequireCallStack' to your function's signature, or discharge the
-- constraint using 'provideCallStack'.
--
-- @since 0.1.0.0
type RequireCallStackImpl = ?provideCallStack :: ()

-- | Raise an 'Control.Exception.ErrorCall' and incur a 'RequireCallStack'
-- constraint while you do so. This
--
-- @since 0.1.0.0
errorRequireCallStack :: RequireCallStack => String -> x
errorRequireCallStack = error

instance TypeError ('Text "Add RequireCallStack to your function context or use provideCallStack") => IP "provideCallStack" ()

-- | Satisfy a 'RequireCallStack' constraint for the given block. Can be
-- used instead of propagating a 'RequireCallStack' up the call graph.
--
-- Usage:
--
-- @
-- main :: `IO` ()
-- main = do
--   `provideCallStack` `$` do
--       `errorRequireCallStack` "hello"
-- @
--
-- If you have the @ImplicitParams@ language extension you can also define the
-- @?provideCallStack@ implicit parameter instead of using `provideCallStack`:
--
-- @
-- main :: `IO` ()
-- main = do
--   let ?provideCallStack = ()
--   `errorRequireCallStack` "hello"
-- @
--
-- Note how @main@ does not have a 'HasCallStack' or 'RequireCallStack'
-- constraint. This function eliminates them, so that
-- 'errorRequireCallStack' can be called without compilation error.
--
-- @since 0.1.0.0
provideCallStack :: (RequireCallStackImpl => r) -> r
provideCallStack r = r
  where
    ?provideCallStack = ()
