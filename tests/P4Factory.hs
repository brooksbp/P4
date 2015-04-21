module P4Factory
  ( p4FactoryTests
  ) where

import Test.HUnit

import Language.P4

p4FactoryTests :: Test
p4FactoryTests = test [
    parseTest' "tests/p4factory/basic_routing/basic_routing.i.p4"
  , parseTest' "tests/p4factory/simple_router/simple_router.i.p4"
  , parseTest' "tests/p4factory/dc_example/dc_example.i.p4"
  , parseTest' "tests/p4factory/sai_p4/sai_p4.i.p4"
  ]


parseAndFixup :: FilePath -> String -> Either String Program
parseAndFixup fileName input = do
  ast <- parseProgram fileName input
  fixupSEMAs ast

parseTest' :: FilePath -> Test
parseTest' file = file ~: do
  contents <- readFile file
  let (b,s) = case parseAndFixup file contents of
        Left err -> (False, err)
        Right _ -> (True, "")
  assertBool s b

parseTest :: FilePath -> Test
parseTest file = file ~: do
  contents <- readFile file
  let (b,s) = case parseProgram file contents of
        Left err -> (False, err)
        Right _ -> (True, "")
  assertBool s b
