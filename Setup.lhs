#! /usr/bin/env runhaskell

> import Distribution.Simple
> import System.Cmd
> tests _ _ _ _ = system "runhaskell src/Tests.hs" >> return ()
> main = defaultMainWithHooks (simpleUserHooks {runTests = tests})

