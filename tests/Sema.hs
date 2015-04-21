module Sema
  ( semaTests
  ) where

import Test.HUnit

import Control.Monad.Except
import Language.P4

semaTests :: Test
semaTests = test $ [
    goAST "1" t1i t1e
  , goAST "2" t2i t2e
  , goAST "3" t3i t3e
  , goAST "4" t4i t4e
  , goStr "5" t5i t5e
  , goStr "6" t6i t6e
  , goStr "7" t7i t7e
  ]

goAST name input expected =
  name ~: assertEqual name expected (runExcept $ fixupFleSEMAs input)

goStr name input expected =
  name ~: assertEqual name expected actual
  where
    actual = case parseProgram "" input of
      Left e  -> Left e
      Right a -> runExcept $ fixupRvtSEMAs a

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


t5i = unlines [
    "parser p1 {"
  , "  return rvt_sema;"
  , "}"
  ]
t5e = Left "Cannot resolve RvtSEMA 'rvt_sema'."

t6i = unlines [
    "parser p1 {"
  , "  return select(latest.f3) {"
  , "    default: rvt_sema;"
  , "  }"
  , "}"
  ]
t6e = Left "Cannot resolve RvtSEMA 'rvt_sema'."

t7i = unlines [
    "control cf1 {}"
  , "parser p1 {"
  , "  return select(latest.f3) {"
  , "    default: cf1;"
  , "  }"
  , "}"
  ]
t7e = Right $ Program [
  ControlFunctionDecl "cf1" (ControlBlock [])
  , ParserFunctionDecl "p1" (ParserFunctionBody []
                             (RsReturnSelect
                              [FodrLatest "f3"]
                              [CaseEntry VlDefault (RvtControlFunc "cf1")]))
  ]
