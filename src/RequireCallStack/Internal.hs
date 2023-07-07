{-# language RankNTypes, FlexibleContexts, FlexibleInstances, ImpredicativeTypes, MultiParamTypeClasses, DataKinds, ConstraintKinds #-}

-- | The implementation details in this module are subject to change
-- and breaking without a corresponding PVP version bump. Import at your
-- own risk.
module RequireCallStack.Internal where

import Unsafe.Coerce
import GHC.Stack

-- | If you're running into this class, then you need to add
-- 'RequireCallStack' to your function's signature, or discharge the
-- constraint using 'provideCallStack'.
--
-- I'd like to provide a 'TypeError' instance here with a good message, but
-- unfortunately, I won't be able to do that because it fails early. I need
-- @Unsatisfiable@ in GHC 9.8 for that. So, until I get that, you'll  have
-- to see an error message that smuggles the suggestion in:
--
-- > No instance for 'Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack'
--
-- Which, hey, it is better than nothing!
--
-- @since 0.1.0.0
class Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack

-- | An alias to make referring to
-- 'Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack' easier,
-- since it is a bit of a mouthful.
--
-- If you see this, you probably need to either add 'RequireCallStack' to
-- the function constraints, or you need to call 'provideCallStack' to
-- discharge it.
--
-- @since 0.1.0.0
type RequireCallStackImpl = Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack

-- | An internal detail. This is a specialization of the trick used in the
-- @reflection@ library to reify constraints. It's based on some GHC
-- trickery - notably, that dictionaries become runtime parameters, and
-- a no method dictionary has the same runtime rep as @()@.
--
-- @since 0.1.0.0
newtype MagicCallStack r = MagicCallStack (RequireCallStackImpl => r)

-- | Satisfy a 'RequireCallStack' constraint for the given block. Can be
-- used instead of propagating a 'RequireCallStack' up the call graph.
--
-- Usage:
--
-- @
-- main :: IO ()
-- main = do
--   provideCallStack $ do
--       errorRequireCallStack "hello"
-- @
--
-- Note how 'main' does not have a 'HasCallStack' or 'RequireCallStack'
-- constraint. This function eliminates them, so that
-- 'errorRequireCallStack' can be called without compilation error.
--
-- @since 0.1.0.0
provideCallStack :: HasCallStack => (RequireCallStackImpl => r) -> r
provideCallStack r = (unsafeCoerce (MagicCallStack r) :: () -> r) ()
