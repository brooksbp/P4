module Sema
  ( semaTests
  ) where

import Test.HUnit

import Control.Monad.Except
import Language.P4

semaTests :: Test
semaTests = test $ [
    go "1" t1i t1e
  , go "2" t2i t2e
  , go "3" t3i t3e
  , go "4" t4i t4e
  ]

go name input expected =
  name ~: assertEqual name expected (runExcept $ fixupFleSEMAs input)


t1i = Program [
  FieldListDecl "fl1" [FleSEMA "unknownfieldlist"]
  ]
t1e = Left "Cannot resolve FleSEMA 'unknownfieldlist'."


t2i = Program [
    ScalarInstanceDecl "h1" "i1"
  , FieldListDecl "fl1" [FleSEMA "i1"]
  ]
t2e = Right $ Program [
    ScalarInstanceDecl "h1" "i1"
  , FieldListDecl "fl1" [FleHeaderRef $ HeaderRef "i1" Nothing]
  ]

t3i = Program [
    FieldListDecl "fl1" [Payload]
  , FieldListDecl "fl2" [FleSEMA "fl1"]
  ]
t3e = Right $ Program [
    FieldListDecl "fl1" [Payload]
  , FieldListDecl "fl2" [FleFieldListName "fl1"]
  ]


t4i = Program [
    ScalarInstanceDecl "h1" "i1"
  , FieldListDecl "i1" [Payload]
  , FieldListDecl "fl1" [FleSEMA "i1"]
  ]
t4e = Left "FleSEMA 'i1' ns collision."
