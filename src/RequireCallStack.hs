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
-- Couple this with @annotated-exception@ library for excellent provenance
-- information on all thrown exceptions.
module RequireCallStack
    ( RequireCallStack
    , RequireCallStackImpl
    , ProvideCallStack
    , provideCallStack
    , errorRequireCallStack
    ) where

import GHC.Stack (HasCallStack)
import GHC.Classes (IP(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | This constraint is similar to 'HasCallStack' in that it's presence
-- will capture a stack frame for the call site of the function. Unlike
-- 'HasCallStack', this is not a "magic" constraint that is automagically
-- solved by GHC, which means that calling a function with this constraint
-- will cause GHC to ask you to add this constraint to your own function.
--
-- For example, let's say you have a function @unsafeHead :: 'RequireCallStack' =>
-- [a] -> a@. Then you go to call that function:
--
-- @
-- myCoolFunction :: [Int] -> Int
-- myCoolFunction = unsafeHead
-- @
--
-- GHC will complain about the lack of the 'RequireCallStack' constraint.
-- You will have two options:
--
-- 1. Add the constraint to your functions. This is a good option because
--    it means the callstack from @unsafeHead@ will include the
--    @myCoolFunction@ callsite as well.
--
--    @
--    myCoolFunction :: RequireCallStack => [Int] -> Int
--    myCoolFunction = unsafeHead
--    @
--
-- 2. Use 'provideCallStack' to silence the error. This will truncate the
--    callstack unless you use 'HasCallStack' above. You should only do
--    this if you're confident that you don't need any debugging
--    information from a more complete callstack.
--
--    @
--    myCoolFunction :: [Int] -> Int
--    myCoolFunction = 'provideCallStack' unsafeHead
--    @
--
-- @since 0.1.0.0
type RequireCallStack = (HasCallStack, RequireCallStackImpl)

-- | If you're running into this class, then you need to add
-- 'RequireCallStack' to your function's signature, or discharge the
-- constraint using 'provideCallStack'.
--
-- See 'RequireCallStack' for more information.
--
-- @since 0.1.0.0
type RequireCallStackImpl = ?provideCallStack :: ProvideCallStack

-- | The constructor for this type is intentionally not exported
data ProvideCallStack = ProvideCallStack

-- | Raise an 'Control.Exception.ErrorCall' and incur a 'RequireCallStack'
-- constraint while you do so. This variant will ensure that callers of
-- unsafe functions are required to provide a callstack until explicitly
-- cut off with 'provideCallStack'.
--
-- @since 0.1.0.0
errorRequireCallStack :: RequireCallStack => String -> x
errorRequireCallStack = error

instance TypeError ('Text "Add RequireCallStack to your function context or use provideCallStack") => IP "provideCallStack" ProvideCallStack

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
-- Note how @main@ does not have a 'HasCallStack' or 'RequireCallStack'
-- constraint. This function eliminates them, so that
-- 'errorRequireCallStack' can be called without compilation error.
--
-- @since 0.1.0.0
provideCallStack :: (RequireCallStackImpl => r) -> r
provideCallStack r = r
  where
    ?provideCallStack = ProvideCallStack
