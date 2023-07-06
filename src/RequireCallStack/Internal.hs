{-# language RankNTypes, FlexibleContexts, FlexibleInstances, ImpredicativeTypes, MultiParamTypeClasses, DataKinds, ConstraintKinds #-}

module RequireCallStack.Internal where

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
class Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack
