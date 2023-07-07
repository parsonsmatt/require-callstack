{-# language FlexibleContexts #-}

module Main (main) where

import RequireCallStack (RequireCallStack, provideCallStack)
import Control.Exception

panic :: RequireCallStack => String -> IO a
panic = error

foo :: RequireCallStack => Int -> IO String
foo _ = panic "foo"

bar :: RequireCallStack => Int -> IO String
bar = foo

baz :: Int -> IO String
baz = provideCallStack bar

main :: IO ()
main = do
    -- won't work, no callstack
   -- panic "asdf"

    provideCallStack $ do
            -- panic "one level of provide callstack"
            pure ()

    Left (ErrorCall "foo") <- try $ baz 3


    -- bar 3

    pure ()
