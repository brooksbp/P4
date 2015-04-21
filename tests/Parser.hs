module Parser
  ( parserTests
  ) where

import Test.HUnit

import Language.P4

parserTests :: Test
parserTests = test $ [
    go "p4/const_value.p4" parseConstValue constValueAST
  , go "p4/header_type_decl.p4" parseProgram headerTypeDeclAST
  , go "p4/instance_decl.p4" parseProgram instanceDeclAST
  , go "p4/field_list_decl.p4" parseProgram fieldListDeclAST
  , go "p4/field_list_calc_decl.p4" parseProgram fieldListCalcDeclAST
  , go "p4/calculated_field_decl.p4" parseProgram calculatedFieldDeclAST
  , go "p4/value_set_decl.p4" parseProgram valueSetDeclAST
  , go "p4/parser_function_decl.p4" parseProgram parserFunctionDeclAST
  , go "p4/parser_exception_decl.p4" parseProgram parserExceptionDeclAST
  , go "p4/counter_decl.p4" parseProgram counterDeclAST
  , go "p4/meter_decl.p4" parseProgram meterDeclAST
  , go "p4/register_decl.p4" parseProgram registerDeclAST
  , go "p4/primitive_action_decl.p4" parseProgram primitiveActionDeclAST
  , go "p4/action_function_decl.p4" parseProgram actionFunctionDeclAST
  , go "p4/action_profile_decl.p4" parseProgram actionProfileDeclAST
  , go "p4/action_selector_decl.p4" parseProgram actionSelectorDeclAST
  , go "p4/table_decl.p4" parseProgram tableDeclAST
  , go "p4/control_function_decl.p4" parseProgram controlFunctionDeclAST
  ]

go file parser expected = file ~: do
  contents <- readFile ("tests/" ++ file)
  assertEqual file (Right expected) (parser file contents)

constValueAST :: [ConstValue]
constValueAST = [
    (ConstValue Nothing Nothing 42)
  , (ConstValue Nothing (Just 16) 42)
  , (ConstValue Nothing Nothing 42)
  , (ConstValue Nothing (Just 0) 0x2a)
  , (ConstValue Nothing (Just 12) 0x100)
  , (ConstValue Nothing (Just 7) 1)
  , (ConstValue (Just Negative) Nothing 5)
  , (ConstValue Nothing Nothing 1000000)
  ]

headerTypeDeclAST :: Program
headerTypeDeclAST = Program [
    HeaderTypeDecl "h1" $ HeaderDecBody [FieldDec "f1" (BwValue 8) []]
    Nothing Nothing
  , HeaderTypeDecl "h2" $ HeaderDecBody [
      FieldDec "f1" (BwValue 1) [Signed]
    , FieldDec "f2" (BwValue 1) [Saturating,Signed]
    ]
    Nothing Nothing
  , HeaderTypeDecl "len_t" $ HeaderDecBody [FieldDec "f1" Variable []]
    (Just (LeBinOp LeAdd
           (LeFieldName "f1")
           (LeBinOp LeShiftL
            (LeValue 1)
            (LeValue 8))))
    Nothing
  , HeaderTypeDecl "ml_t" $ HeaderDecBody [FieldDec "f1" Variable []]
    Nothing (Just 0x10)
  ]

instanceDeclAST :: Program
instanceDeclAST = Program [
    ScalarInstanceDecl "h1" "i1"
  , ArrayInstanceDecl "h1" "i1" 10
  , MetadataInstanceDecl "h1" "i1" Nothing
  , MetadataInstanceDecl "h1" "i1" (Just [("f1", 1)])
  , MetadataInstanceDecl "h1" "i1" (Just [("f1", 1),("f2", 1)])
  ]

fieldListDeclAST :: Program
fieldListDeclAST = Program [
    FieldListDecl "fl1" [Payload]
  , FieldListDecl "fl1" [FleSEMA "fieldlistname"]
  , FieldListDecl "fl1" [FleFieldValue 1, FleFieldValue 2]
  , FieldListDecl "fl1" [
         FleHeaderRef $ HeaderRef "i1" (Just (IValue 0))
       , FleHeaderRef $ HeaderRef "i2" (Just Last)
       ]
  , FieldListDecl "fl1" [
         FleFieldRef $ FieldRef (HeaderRef "i1" Nothing) "f1"
       , FleFieldRef $ FieldRef (HeaderRef "i90" (Just (IValue 0xfff))) "f2"
       ]
  ]

fieldListCalcDeclAST :: Program
fieldListCalcDeclAST = Program [
  FieldListCalcDecl "flc1" ["fl1","fl2"] "crc32" 0xFFFF
  ]

calculatedFieldDeclAST :: Program
calculatedFieldDeclAST = Program [
  CalculatedFieldDecl (FieldRef (HeaderRef "h1" Nothing) "f1") [
       UpdateVerifySpec Update "flcn1" Nothing
     , UpdateVerifySpec Verify "flcn1" (Just $ IfCondValid (Left $ HeaderRef "h1" Nothing))
     , UpdateVerifySpec Verify "flcn1" (Just $ IfCondValid (Left $ HeaderRef "h1" (Just $ IValue 0)))
     , UpdateVerifySpec Verify "flcn1" (Just $ IfCondValid (Right $ FieldRef (HeaderRef "h1" Nothing) "f2"))
     , UpdateVerifySpec Update "flcn1" (Just $ IfCondEq (FieldRef (HeaderRef "h1" (Just Last)) "f3") 0x1010)
     ]
  ]

valueSetDeclAST :: Program
valueSetDeclAST = Program [ValueSetDecl "vs1"]

fr :: String -> String -> FieldRef
fr h = FieldRef (HeaderRef h Nothing)

parserFunctionDeclAST :: Program
parserFunctionDeclAST = Program [
    ParserFunctionDecl "p1" $ ParserFunctionBody [
         EosExtractStmt $ HeaderExtractRef "i1" Nothing
       , EosExtractStmt $ HeaderExtractRef "i1" (Just $ HeiValue 0xff)
       , EosExtractStmt $ HeaderExtractRef "i1" (Just Next)
       ]
    (RsReturnValueType $ RvtSEMA "p2")
  , ParserFunctionDecl "p1" $ ParserFunctionBody [
         EosSetStmt $ SetStmt (fr "h1" "f1") (MeFieldValue 0x01)
       , EosSetStmt $ SetStmt (fr "h1" "f1") (MeFieldOrDataRef $ FodrFieldRef (fr "h1" "f2"))
       , EosSetStmt $ SetStmt (fr "h1" "f1") (MeFieldOrDataRef $ FodrLatest "f3")
       , EosSetStmt $ SetStmt (fr "h1" "f1") (MeFieldOrDataRef $ FodrCurrent 2 3)
       ]
    (RsReturnValueType $ RvtSEMA "p2")
  , ParserFunctionDecl "p1" $ ParserFunctionBody []
    $ RsReturnValueType $ RvtSEMA "p2"
  , ParserFunctionDecl "p1" $ ParserFunctionBody []
    $ RsReturnValueType $ RvtParserExcept "pe1"
  , ParserFunctionDecl "p1" $ ParserFunctionBody []
    $ RsReturnSelect [
      FodrLatest "f3"
    , FodrCurrent 2 3
    ]
    [ CaseEntry VlDefault (RvtSEMA "name")
    , CaseEntry VlDefault (RvtParserExcept "pe1")
    , CaseEntry (VlValueList [VomFieldValue 0x1]) (RvtParserExcept "pe1")
    , CaseEntry (VlValueList [
                      VomFieldValue 0x1
                    , VomFieldValue 0x2
                    ])
      (RvtParserExcept "pe1")
    , CaseEntry (VlValueList [
                    VomFieldValueMask 0x5 0xa
                    ])
      (RvtParserExcept "pe1")
    , CaseEntry (VlValueList [
                      VomValueSetName "vs1"
                    , VomFieldValue 0x1
                    ])
      (RvtParserExcept "pe1")
    ]
  ]

parserExceptionDeclAST :: Program
parserExceptionDeclAST = Program [
    ParserExceptionDecl "pe1" [] Drop
  , ParserExceptionDecl "pe1" [] (Return "cfn1")
  , ParserExceptionDecl "pe1" [
    SetStmt (fr "h1" "f1") (MeFieldValue 0x01)
    ] Drop
  , ParserExceptionDecl "pe1" [
      SetStmt (fr "h1" "f1") (MeFieldValue 0x01)
    , SetStmt (fr "h1" "f1") (MeFieldOrDataRef $ FodrLatest "f3")
    ] Drop
  ]

counterDeclAST :: Program
counterDeclAST = Program [
    CounterDecl "c1" Bytes (Just $ DirectAttrib "t1") (Just 4) (Just 2) True
  , CounterDecl "c2" Packets Nothing Nothing Nothing False
  ]

meterDeclAST :: Program
meterDeclAST = Program [
    MeterDecl "m1" Bytes (fr "h1" "f1") (Just $ StaticAttrib "t1") (Just 1)
  , MeterDecl "m2" Packets (fr "h1" "f1") Nothing Nothing
  ]

registerDeclAST :: Program
registerDeclAST = Program [
    RegisterDecl "r1" (Width 4) (Just $ StaticAttrib "t1") (Just 8) [Signed,Saturating]
  , RegisterDecl "r2" (Layout "h1") Nothing Nothing []
  ]

primitiveActionDeclAST :: Program
primitiveActionDeclAST = Program [
    PrimitiveActionDecl "pa1" []
  , PrimitiveActionDecl "pa1" ["p1", "p2", "p3"]
  ]

actionFunctionDeclAST :: Program
actionFunctionDeclAST = Program [
    ActionFunctionDecl (ActionHeader "a1" []) [ActionStmt "a2" []]
  , ActionFunctionDecl (ActionHeader "a1" ["p1", "p2"]) [
         ActionStmt "a2" [
              AaFieldValue 0xa
            , AaFieldRef (fr "h1" "f1")
            , AaSEMA "h1"
            ]
       , ActionStmt "a3" [AaFieldValue 1]
       ]
  ]

actionProfileDeclAST :: Program
actionProfileDeclAST = Program [
    ActionProfileDecl "ap1" ["a1", "a2"] (Just 1) (Just "s1")
  , ActionProfileDecl "ap2" ["a1"] Nothing Nothing
  ]

actionSelectorDeclAST :: Program
actionSelectorDeclAST = Program [
  ActionSelectorDecl "as1" "flcn1"
  ]

tableDeclAST :: Program
tableDeclAST = Program [
    TableDecl "t1" (Just [
                         FieldMatch (FomrHeaderRef $ HeaderRef"h1" Nothing) Exact
                       , FieldMatch (FomrFieldRef $ fr "h1" "f1") Ternary
                       , FieldMatch (FomrFieldRefMask (fr "h1" "f1") 0x8) Lpm
                       ])
    (ActionSpecification ["a1", "a2"])
    (Just 8) (Just 4096) (Just 128) (Just True)
  , TableDecl "t2" Nothing (ActionProfileSpecification "ap1") Nothing Nothing Nothing Nothing
  ]

controlFunctionDeclAST :: Program
controlFunctionDeclAST = Program [
    ControlFunctionDecl "cf1" $ ControlBlock []
  , ControlFunctionDecl "cf1" $ ControlBlock [
         ApplyTableCall "t1"
       , ApplyTableCall "t2"
       ]
  , ControlFunctionDecl "cf1" $ ControlBlock [
       ApplyAndSelectBlock "t1" [
            ActionCase (AodActionName "a1") (ControlBlock [ApplyTableCall "t1"])
          , ActionCase AodDefault (ControlBlock [])
          ]
       ]
  , ControlFunctionDecl "cf2" $ ControlBlock [
       ApplyAndSelectBlock "t1" [
            HitMissCase Hit  (ControlBlock [ApplyTableCall "t7"])
          , HitMissCase Miss (ControlBlock [ApplyTableCall "t8"])
          ]
       ]
  , ControlFunctionDecl "cf3" $ ControlBlock [CsControlFunctionName "cf2"]
  , ControlFunctionDecl "cf4" $ ControlBlock [
       CsIfElseStmt $ IfElseStmt (BeBool True)
       (ControlBlock [CsControlFunctionName "cf1"])
       (Just $ ElseIfElseStmt $ IfElseStmt (BeBool True)
        (ControlBlock [CsControlFunctionName "cf2"])
        (Just $ ElseControlBlock
         (ControlBlock [CsControlFunctionName "cf3"])))
       ]
  ]
