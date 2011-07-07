#! /usr/bin/env runhaskell

import Distribution.Simple
import Distribution.PackageDescription(PackageDescription)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
import System.Cmd(system)
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks (simpleUserHooks {runTests = runAllTests, postHaddock = runHlint})

runAllTests a b pd lb = do
    system ( "runhaskell -i./src ./testsuite/tests/CardTests.hs") >> return()
    system ( "runhaskell -i./src ./testsuite/tests/PlayerTests.hs") >> return()
    system ( "runhaskell -i./src ./testsuite/tests/GameTests.hs") >> return()
    system ( "runhaskell -i./src ./testsuite/tests/PlayerCircleTests.hs") >> return()

runHlint a b pd lb = do
    system ("hlint ./src/*.hs --report=./dist/doc/html/haskellhead/haskellhead-hlint.html") >> return ()
    system ("hlint ./testsuite/tests/*.hs --report=./dist/doc/html/haskellhead/tests-hlint.html") >> return ()  
