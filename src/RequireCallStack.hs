{-# language RankNTypes, FlexibleContexts, FlexibleInstances, ImpredicativeTypes, MultiParamTypeClasses, DataKinds, ConstraintKinds #-}

module RequireCallStack
    ( RequireCallStack
    , RequireCallStackImpl
    , provideCallStack
    ) where

import GHC.Stack
import Unsafe.Coerce
import GHC.TypeLits

import RequireCallStack.Internal

-- | An alias to make referring to this easier.
type RequireCallStackImpl = Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack

-- | This constraint is similar to 'HasCallStack' in that it's presence
-- will capture a stack frame for the call site of the function. This helps
-- to preserve callstack provenance, which
type RequireCallStack = (HasCallStack, RequireCallStackImpl)

newtype MagicCallStack r = MagicCallStack (RequireCallStackImpl => r)

provideCallStack :: HasCallStack => (RequireCallStackImpl => r) -> r
provideCallStack r = (unsafeCoerce (MagicCallStack r) :: () -> r) ()
