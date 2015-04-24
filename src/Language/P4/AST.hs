module Language.P4.AST
 ( Program(..)
 , Decl(..)

 , ConstValue(..)
 , Sign(..)
 , c2i

 , FieldValue

 , HeaderTypeName
 , FieldName
 , FieldListName
 , FieldListCalcName
 , InstanceName
 , AlgorithmName
 , ValueSetName
 , ParserStateName
 , ControlFunctionName
 , ParserExceptionName
 , CounterName
 , TableName
 , MeterName
 , RegisterName
 , ActionName
 , ParamName
 , ActionProfileName
 , SelectorName

 , HeaderDecBody(..)
 , LeBinOp(..)
 , LengthExp(..)
 , FieldDec(..)
 , BitWidth(..)
 , SignedOrSaturating(..)

 , FieldListEntry(..)
 , FieldRef(..)
 , HeaderRef(..)
 , Index(..)

 , UpdateVerifySpec(..)
 , UpdateOrVerify(..)
 , IfCond(..)

 , ParserFunctionBody(..)
 , ExtractOrSetStmt(..)
 , SetStmt(..)
 , HeaderExtractRef(..)
 , HeaderExtractIndex(..)
 , MetadataExpr(..)
 , ReturnStmt(..)
 , ReturnValueType(..)
 , CaseEntry(..)
 , ValueList(..)
 , ValueOrMasked(..)
 , FieldOrDataRef(..)

 , ReturnOrDrop(..)

 , BytesOrPackets(..)
 , DirectOrStatic(..)

 , WidthOrLayout(..)
 , RegisterRef(..)

 , ActionHeader(..)
 , ActionStmt(..)
 , ActionArg(..)

 , FieldMatch(..)
 , FieldOrMaskedRef(..)
 , FieldMatchType(..)
 , TableActions(..)

 , ControlBlock(..)
 , ControlStmt(..)
 , CaseList(..)
 , ActionOrDefault(..)
 , HitOrMiss(..)
 , IfElseStmt(..)
 , ElseBlock(..)
 , BoolExpr(..)
 , BoolOp(..)
 , Exp(..)
 , BinOp(..)
 , UnOp(..)
 , RelOp(..)

 ) where

data Program
  = Program [Decl]
  deriving (Eq, Show)

data Decl
  = HeaderTypeDecl HeaderTypeName HeaderDecBody
  | ScalarInstanceDecl HeaderTypeName InstanceName
  | ArrayInstanceDecl HeaderTypeName InstanceName ConstValue
  | MetadataInstanceDecl HeaderTypeName InstanceName (Maybe [(FieldName, FieldValue)])
  | FieldListDecl FieldListName [FieldListEntry]
  | FieldListCalcDecl FieldListCalcName [FieldListName] AlgorithmName ConstValue
  | CalculatedFieldDecl FieldRef [UpdateVerifySpec]
  | ValueSetDecl ValueSetName
  | ParserFunctionDecl ParserStateName ParserFunctionBody
  | ParserExceptionDecl ParserExceptionName [SetStmt] ReturnOrDrop
  | CounterDecl CounterName BytesOrPackets (Maybe DirectOrStatic) (Maybe ConstValue) (Maybe ConstValue) Bool
  | MeterDecl MeterName BytesOrPackets FieldRef (Maybe DirectOrStatic) (Maybe ConstValue)
  | RegisterDecl RegisterName WidthOrLayout (Maybe DirectOrStatic) (Maybe ConstValue) [SignedOrSaturating]
  | PrimitiveActionDecl ActionName [ParamName]
  | ActionFunctionDecl ActionHeader [ActionStmt]
  | ActionProfileDecl ActionProfileName [ActionName] (Maybe ConstValue) (Maybe SelectorName)
  | ActionSelectorDecl SelectorName FieldListCalcName
  | TableDecl TableName (Maybe [FieldMatch]) TableActions (Maybe ConstValue) (Maybe ConstValue) (Maybe ConstValue) (Maybe Bool)
  | ControlFunctionDecl ControlFunctionName ControlBlock
  deriving (Eq, Show)

data Sign = Positive | Negative deriving (Eq, Show)

data ConstValue
  = ConstValue (Maybe Sign) (Maybe Integer) Integer
  deriving (Eq, Show)

c2i :: ConstValue -> Integer
c2i (ConstValue sign _ val) = case sign of
  Nothing -> val
  Just Positive -> val
  Just Negative -> -val

cBinOp :: ConstValue -> ConstValue -> (Integer -> Integer -> Integer) -> ConstValue
cBinOp a b op = let value = c2i a `op` c2i b
                    sign  = if value < 0 then Just Negative else Nothing in
                 ConstValue sign Nothing (abs value)

instance Num ConstValue where
  a + b = cBinOp a b (+)
  a - b = cBinOp a b (-)
  a * b = cBinOp a b (*)
  abs (ConstValue (Just Negative) w v) = ConstValue Nothing w (abs v)
  abs x = x
  signum = error "undefined"
  fromInteger = ConstValue Nothing Nothing

instance Ord ConstValue where
  compare a b = compare (c2i a) (c2i b)

type FieldValue = ConstValue

type HeaderTypeName = String
type FieldName = String
type FieldListName = String
type FieldListCalcName = String
type InstanceName = String
type AlgorithmName = String
type ValueSetName = String
type ParserStateName = String
type ControlFunctionName = String
type ParserExceptionName = String
type CounterName = String
type TableName = String
type MeterName = String
type RegisterName = String
type ActionName = String
type ParamName = String
type ActionProfileName = String
type SelectorName = String

data HeaderDecBody
  = HeaderDecBody [FieldDec] (Maybe LengthExp) (Maybe ConstValue)
  deriving (Eq, Show)

data LeBinOp = LeAdd | LeSub | LeMul | LeShiftL | LeShiftR deriving (Eq, Show)

data LengthExp
  = LeValue ConstValue
  | LeFieldName FieldName
  | LeBinOp LeBinOp LengthExp LengthExp
  deriving (Eq, Show)

data FieldDec
  = FieldDec FieldName BitWidth [SignedOrSaturating]
  deriving (Eq, Show)

data BitWidth
  = BwValue ConstValue
  | Variable
  deriving (Eq, Show)

data SignedOrSaturating = Signed | Saturating deriving (Eq, Show)

data FieldListEntry
  = FleHeaderRef HeaderRef
  | FleFieldRef FieldRef
  | FleFieldValue FieldValue
  | FleFieldListName FieldListName
  | FleSEMA String
  | Payload
  deriving (Eq, Show)

data FieldRef
  = FieldRef HeaderRef FieldName
  deriving (Eq, Show)

data HeaderRef
  = HeaderRef InstanceName (Maybe Index)
  deriving (Eq, Show)

data Index
  = IValue ConstValue
  | Last
  deriving (Eq, Show)

data UpdateVerifySpec
  = UpdateVerifySpec UpdateOrVerify FieldListCalcName (Maybe IfCond)
  deriving (Eq, Show)

data UpdateOrVerify = Update | Verify deriving (Eq, Show)

data IfCond
  = IfCondValid (Either HeaderRef FieldRef)
  | IfCondEq FieldRef FieldValue
  deriving (Eq, Show)

data ParserFunctionBody
  = ParserFunctionBody [ExtractOrSetStmt] ReturnStmt
  deriving (Eq, Show)

data ExtractOrSetStmt
  = EosExtractStmt HeaderExtractRef
  | EosSetStmt SetStmt
  deriving (Eq, Show)

data SetStmt
  = SetStmt FieldRef MetadataExpr
  deriving (Eq, Show)

data HeaderExtractRef
  = HeaderExtractRef InstanceName (Maybe HeaderExtractIndex)
  deriving (Eq, Show)

data HeaderExtractIndex
  = HeiValue ConstValue
  | Next
  deriving (Eq, Show)

data MetadataExpr
  = MeFieldValue FieldValue
  | MeFieldOrDataRef FieldOrDataRef
  deriving (Eq, Show)

data ReturnStmt
  = RsReturnValueType ReturnValueType
  | RsReturnSelect [FieldOrDataRef] [CaseEntry]
  deriving (Eq, Show)

data ReturnValueType
  = RvtParserState ParserStateName
  | RvtControlFunc ControlFunctionName
  | RvtParserExcept ParserExceptionName
  | RvtSEMA String
  deriving (Eq, Show)

data CaseEntry
  = CaseEntry ValueList ReturnValueType
  deriving (Eq, Show)

data ValueList
  = VlValueList [ValueOrMasked]
  | VlDefault
  deriving (Eq, Show)

data ValueOrMasked
  = VomFieldValue FieldValue
  | VomFieldValueMask FieldValue FieldValue
  | VomValueSetName ValueSetName
  deriving (Eq, Show)

data FieldOrDataRef
  = FodrFieldRef FieldRef
  | FodrLatest FieldName
  | FodrCurrent ConstValue ConstValue
  deriving (Eq, Show)

data ReturnOrDrop
  = Return ControlFunctionName
  | Drop
  deriving (Eq, Show)

data BytesOrPackets = Bytes | Packets deriving (Eq, Show)

data DirectOrStatic
  = DirectAttrib TableName
  | StaticAttrib TableName
  deriving (Eq, Show)

data WidthOrLayout
  = Width ConstValue
  | Layout HeaderTypeName
  deriving (Eq, Show)

data RegisterRef
  = RegisterRef RegisterName ConstValue (Maybe FieldName)
  deriving (Eq, Show)

data ActionHeader
  = ActionHeader ActionName [ParamName]
  deriving (Eq, Show)

data ActionStmt
  = ActionStmt ActionName [ActionArg]
  deriving (Eq, Show)

data ActionArg
  = AaHeaderRef HeaderRef
  | AaFieldRef FieldRef
  | AaFieldValue FieldValue
  | AaParamName ParamName
  | AaSEMA String
  deriving (Eq, Show)

data FieldMatch
  = FieldMatch FieldOrMaskedRef FieldMatchType
  deriving (Eq, Show)

data FieldOrMaskedRef
  = FomrHeaderRef HeaderRef
  | FomrFieldRef FieldRef
  | FomrFieldRefMask FieldRef ConstValue
  deriving (Eq, Show)

data FieldMatchType = Exact | Ternary | Lpm | Range | Valid deriving (Eq, Show)

data TableActions
  = ActionSpecification [ActionName]
  | ActionProfileSpecification ActionProfileName
  deriving (Eq, Show)

data ControlBlock
  = ControlBlock [ControlStmt]
  deriving (Eq, Show)

data ControlStmt
  = ApplyTableCall TableName
  | ApplyAndSelectBlock TableName [CaseList]
  | CsIfElseStmt IfElseStmt
  | CsControlFunctionName ControlFunctionName
  deriving (Eq, Show)

data CaseList
  = ActionCase ActionOrDefault ControlBlock
  | HitMissCase HitOrMiss ControlBlock
  deriving (Eq, Show)

data ActionOrDefault
  = AodActionName ActionName
  | AodDefault
  deriving (Eq, Show)

data HitOrMiss = Hit | Miss deriving (Eq, Show)

data IfElseStmt
  = IfElseStmt BoolExpr ControlBlock (Maybe ElseBlock)
  deriving (Eq, Show)

data ElseBlock
  = ElseControlBlock ControlBlock
  | ElseIfElseStmt IfElseStmt
  deriving (Eq, Show)

data BoolExpr
  = BeValidHeaderRef HeaderRef
  | BeBoolOp BoolOp BoolExpr BoolExpr
  | BeNot BoolExpr
  | BeRelOp RelOp Exp Exp
  | BeBool Bool
  deriving (Eq, Show)

data BoolOp = BoolOr | BoolAnd deriving (Eq, Show)

data Exp
  = BinOp BinOp Exp Exp
  | UnOp UnOp Exp
  | FRef FieldRef
  | Value ConstValue
  deriving (Eq, Show)

data BinOp = Add | Mul | Sub | ShiftL | ShiftR | And | Or | Xor deriving (Eq, Show)

data UnOp = UNot | UNeg deriving (Eq, Show)

data RelOp = Gt | Gte | Eq | Lte | Lt | Neq deriving (Eq, Show)
