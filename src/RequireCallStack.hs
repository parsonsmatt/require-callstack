{-# language RankNTypes, FlexibleContexts, FlexibleInstances, ImpredicativeTypes, MultiParamTypeClasses, DataKinds, ConstraintKinds #-}

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
-- foo :: RequireCallStack => Int -> String
-- foo = error "oh no"
--
-- bar :: Int -> String
-- bar i = provideCallStack $ foo i
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

import GHC.Stack
import Unsafe.Coerce
import GHC.TypeLits
import Control.Exception (throw)

import RequireCallStack.Internal

-- | An alias to make referring to this easier.
--
-- @since 0.1.0.0
type RequireCallStackImpl = Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack

-- | This constraint is similar to 'HasCallStack' in that it's presence
-- will capture a stack frame for the call site of the function. This helps
-- to preserve callstack provenance, which
--
-- @since 0.1.0.0
type RequireCallStack = (HasCallStack, RequireCallStackImpl)

-- | Raise an 'ErrorCall' and incur a 'RequireCallStack' constraint while
-- you do so. This
--
-- @since 0.1.0.0
errorRequireCallStack :: RequireCallStack => String -> x
errorRequireCallStack = error
