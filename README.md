# `require-callstack`

Haskell has opt-in call stacks through the use of the `HasCallStack` constraint. 
One unfortunate aspect of this design is that the resulting `CallStack` can be truncated if any function in the call list omits the constraint.

```haskell
foo :: HasCallStack => Int -> String
foo = error "oh no"

bar :: HasCallStack => Int -> String
bar = foo . negate

baz :: Int -> String
baz = bar . (* 2)

main :: IO ()
main = do
    print $ baz 5
```

Running this code will fail with an `ErrorCall "oh no"` exception.
The attached `CallStack` will only mention `foo` and `bar` - `baz` *will not* be present, nor will `main`.
A truncated `CallStack` isn't nearly as useful as you might like.

One solution is the [`annotated-exception`](https://www.stackage.org/lts-21.1/package/annotated-exception-0.2.0.4) library, which can attach `CallStack` to any thrown exception, and `catch` is guaranteed to add a stack frame at the catch-site for any exception that passes through.
However, it's *still* nice to have `HasCallStack` entries on functions - then you get the name of the function, which makes diagnosing an error report easier.

This library introduces a type `RequireCallStack`. 
Unlike `HasCallStack`, this isn't automagically solved - if you call a function that has `RequireCallStack` in the constraint, you must either call `provideCallStack` to discharge the constraint, or add `RequireCallStack` to the signature of the function you're defining.

```haskell
panic :: RequireCallStack => String -> a
panic = error

foo :: RequireCallStack => Int -> String
foo = panic "oh no"

bar :: RequireCallStack => Int -> String
bar = foo . negate

baz :: Int -> String
baz = bar . (* 2)

main :: IO ()
main = do
    print $ baz 5
```

This code will fail with a compile-time error:

> ```
> /home/matt/Projects/require-callstack/test/Main.hs:30:5: error: [GHC-39999]
>     • No instance for ‘RequireCallStack.Internal.Add_RequireCallStack_ToFunctionContext_OrUse_provideCallStack’
>         arising from a use of ‘bar’
>         ....
>    |        
> 30 |     bar . (* 2)
>    |     ^^^
> ```

The error message, read carefully, will tell you how to solve the issue.
If we then write:

```haskell
panic :: RequireCallStack => String -> a
panic = error

foo :: RequireCallStack => Int -> String
foo = panic "oh no"

bar :: RequireCallStack => Int -> String
bar = foo . negate

baz :: RequireCallStack => Int -> String
baz = bar . (* 2)

main :: IO ()
main = provideCallStack $ do
    print $ baz 5
```

Then the code compiles and works as expected.
