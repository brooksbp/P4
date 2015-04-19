module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Parser
import Sema
import P4Factory

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Parser tests" $ hUnitTestToTests parserTests
        , testGroup "Sema tests" $ hUnitTestToTests semaTests
        , testGroup "P4Factory tests" $ hUnitTestToTests p4FactoryTests
        ]
