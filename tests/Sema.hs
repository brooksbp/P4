module Sema
  ( semaTests
  ) where

import Test.HUnit

import Control.Monad.Except
import Language.P4

semaTests :: Test
semaTests = test $ [
    goAST fixupFleSEMAs "1" t1i t1e
  , goAST fixupFleSEMAs "2" t2i t2e
  , goAST fixupFleSEMAs "3" t3i t3e
  , goAST fixupFleSEMAs "4" t4i t4e
  , goStr fixupRvtSEMAs "5" t5i t5e
  , goStr fixupRvtSEMAs "6" t6i t6e
  , goStr fixupRvtSEMAs "7" t7i t7e
  , goStr fixupAaSEMAs  "8" t8i t8e
  ]

goAST semaFn name input expected =
  name ~: assertEqual name expected (runExcept $ semaFn input)

goStr semaFn name input expected =
  name ~: assertEqual name expected actual
  where
    actual = case parseProgram "" input of
      Left e  -> Left e
      Right a -> runExcept $ semaFn a

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

t8i = unlines [
    "header h1 i1;"
  , "action a1(p1, p2) {"
  , "  a2(i1, pname);"
  , "}"
  ]
t8e = Right $ Program [
    ScalarInstanceDecl "h1" "i1"
  , ActionFunctionDecl (ActionHeader "a1" ["p1","p2"])
    [ActionStmt "a2" [
          AaHeaderRef (HeaderRef "i1" Nothing)
        , AaParamName "pname"
        ]
    ]
  ]
