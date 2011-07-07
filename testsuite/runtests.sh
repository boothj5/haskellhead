#!/bin/sh

echo
echo Running GameTests...
runhaskell -i../src/ tests/GameTests.hs

echo
echo Running PlayerTests...
runhaskell -i../src/ tests/PlayerTests.hs

echo
echo Running CardTests...
runhaskell -i../src/ tests/CardTests.hs

echo
echo Running PlayerCircleTests...
runhaskell -i../src/ tests/PlayerCircleTests.hs
