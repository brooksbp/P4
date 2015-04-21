-- -*-haskell-*-
{
module Language.P4.Parser.Parse
  ( program
  , constValue
  ) where

import Control.Monad.Except
import Data.Char (digitToInt)
import Data.List (reverse, isPrefixOf)

import Language.P4.AST
import Language.P4.Parser.Tokens
}

%name program Program
%name constValue ConstValueTop

%tokentype { Token }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%expect 0

%token

"action"            { Token KW_action _ _ }
"action_profile"    { Token KW_action_profile _ _ }
"action_selector"   { Token KW_action_selector _ _ }
"actions"           { Token KW_actions _ _ }
"algorithm"         { Token KW_algorithm _ _ }
"and"               { Token KW_and _ _ }
"apply"             { Token KW_apply _ _ }
"attributes"        { Token KW_attributes _ _ }
"bytes"             { Token KW_bytes _ _ }
"calculated_field"  { Token KW_calculated_field _ _ }
"control"           { Token KW_control _ _ }
"counter"           { Token KW_counter _ _ }
"current"           { Token KW_current _ _ }
"default"           { Token KW_default _ _ }
"direct"            { Token KW_direct _ _ }
"dynamic_action_selection" { Token KW_dynamic_action_selection _ _ }
"else"              { Token KW_else _ _ }
"exact"             { Token KW_exact _ _ }
"extract"           { Token KW_extract _ _ }
"false"             { Token KW_false _ _ }
"field_list"        { Token KW_field_list _ _ }
"field_list_calculation" { Token KW_field_list_calculation _ _ }
"fields"            { Token KW_fields _ _ }
"header"            { Token KW_header _ _ }
"header_type"       { Token KW_header_type _ _ }
"hit"               { Token KW_hit _ _ }
"if"                { Token KW_if _ _ }
"input"             { Token KW_input _ _ }
"instance_count"    { Token KW_instance_count _ _ }
"last"              { Token KW_last _ _ }
"latest"            { Token KW_latest _ _ }
"layout"            { Token KW_layout _ _ }
"length"            { Token KW_length _ _ }
"lpm"               { Token KW_lpm _ _ }
"mask"              { Token KW_mask _ _ }
"max_length"        { Token KW_max_length _ _ }
"max_size"          { Token KW_max_size _ _ }
"metadata"          { Token KW_metadata _ _ }
"meter"             { Token KW_meter _ _ }
"min_size"          { Token KW_min_size _ _ }
"min_width"         { Token KW_min_width _ _ }
"miss"              { Token KW_miss _ _ }
"next"              { Token KW_next _ _ }
"not"               { Token KW_not _ _ }
"or"                { Token KW_or _ _ }
"output_width"      { Token KW_output_width _ _ }
"packets"           { Token KW_packets _ _ }
"parse_error"       { Token KW_parse_error _ _ }
"parser"            { Token KW_parser _ _ }
"parser_drop"       { Token KW_parser_drop _ _ }
"parser_exception"  { Token KW_parser_exception _ _ }
"parser_value_set"  { Token KW_parser_value_set _ _ }
"payload"           { Token KW_payload _ _ }
"primitive_action"  { Token KW_primitive_action _ _ }
"range"             { Token KW_range _ _ }
"reads"             { Token KW_reads _ _ }
"register"          { Token KW_register _ _ }
"result"            { Token KW_result _ _ }
"return"            { Token KW_return _ _ }
"saturating"        { Token KW_saturating _ _ }
"select"            { Token KW_select _ _ }
"selection_key"     { Token KW_selection_key _ _ }
"set_metadata"      { Token KW_set_metadata _ _ }
"signed"            { Token KW_signed _ _ }
"size"              { Token KW_size _ _ }
"static"            { Token KW_static _ _ }
"support_timeout"   { Token KW_support_timeout _ _ }
"table"             { Token KW_table _ _ }
"ternary"           { Token KW_ternary _ _ }
"true"              { Token KW_true _ _ }
"type"              { Token KW_type _ _ }
"update"            { Token KW_update _ _ }
"valid"             { Token KW_valid _ _ }
"verify"            { Token KW_verify _ _ }
"width"             { Token KW_width _ _ }

identifier          { Token Identifier _ _ }

unsignedValue       { Token Lit_unsigned _ _ }

"'"               { Token Sym_s_quote _ _ }
"("               { Token Sym_paren_l _ _ }
")"               { Token Sym_paren_r _ _ }
"["               { Token Sym_bracket_l _ _ }
"]"               { Token Sym_bracket_r _ _ }
"{"               { Token Sym_brace_l _ _ }
"}"               { Token Sym_brace_r _ _ }
":"               { Token Sym_colon _ _ }
";"               { Token Sym_semi _ _ }
","               { Token Sym_comma _ _ }
"+"               { Token Sym_plus _ _ }
"-"               { Token Sym_dash _ _ }
"*"               { Token Sym_aster _ _ }
"<"               { Token Sym_lt _ _ }
">"               { Token Sym_gt _ _ }
"."               { Token Sym_dot _ _ }
"&"               { Token Sym_amp _ _ }
"|"               { Token Sym_bar _ _ }
"^"               { Token Sym_hat _ _ }
"~"               { Token Sym_tilde _ _ }

"=="              { Token Sym_eq _ _ }
"<<"              { Token Sym_lt_lt _ _ }
">>"              { Token Sym_gt_gt _ _ }
">="              { Token Sym_gt_eq _ _ }
"<="              { Token Sym_lt_eq _ _ }
"!="              { Token Sym_bang_eq _ _ }

%left "or"
%left "and"
%left "|"
%left "^"
%left "&"
%left "==" "!="
%left ">" ">=" "<" "<="
%left "<<" ">>"
%left "+" "-"
%left "*"
%left UMinus "~" "not"
%left Sign "'"

%%

Program :: { Program }
: list(Decl)  { Program $1 }

Decl :: { Decl }
: "header_type" HeaderTypeName "{" HeaderDecBody "}"  { HeaderTypeDecl $2 $4 }
| "header" HeaderTypeName InstanceName ";"  { ScalarInstanceDecl $2 $3 }
| "header" HeaderTypeName InstanceName "[" ConstValue "]" ";"  { ArrayInstanceDecl $2 $3 $5 }
| "metadata" HeaderTypeName InstanceName MaybeMetadataInitializer ";"  { MetadataInstanceDecl $2 $3 $4 }
| "field_list" FieldListName "{" list1(FieldListEntry) "}"  { FieldListDecl $2 $4 }
| "field_list_calculation" FieldListCalcName "{" "input" "{" list1(fst(FieldListName,";")) "}" "algorithm" ":" AlgorithmName ";" "output_width" ":" ConstValue ";" "}"  { FieldListCalcDecl $2 $6 $10 $14 }
| "calculated_field" FieldRef "{" list1(UpdateVerifySpec) "}"  { CalculatedFieldDecl $2 $4 }
| "parser_value_set" ValueSetName ";"  { ValueSetDecl $2 }
| "parser" ParserStateName "{" ParserFunctionBody "}"  { ParserFunctionDecl $2 $4 }
| "parser_exception" ParserExceptionName "{" list(SetStmt) ReturnOrDrop ";" "}"  { ParserExceptionDecl $2 $4 $5 }
| "counter" CounterName "{" "type" ":" BytesOrPackets ";" opt(DirectOrStaticDecl) opt(InstanceCountDecl) opt(MinWidthDecl) SaturatingDecl "}"  { CounterDecl $2 $6 $8 $9 $10 $11 }
| "meter" MeterName "{" "type" ":" BytesOrPackets ";" "result" ":" FieldRef ";" opt(DirectOrStaticDecl) opt(InstanceCountDecl) "}"  { MeterDecl $2 $6 $10 $12 $13 }
| "register" RegisterName "{" WidthOrLayoutDecl opt(DirectOrStaticDecl) opt(InstanceCountDecl) AttributeList "}"  { RegisterDecl $2 $4 $5 $6 $7 }
| "primitive_action" ActionName "(" ParamList ")" ";"  { PrimitiveActionDecl $2 $4 }
| "action" ActionHeader "{" list(ActionStmt) "}"  { ActionFunctionDecl $2 $4 }
| "action_profile" ActionProfileName "{" ActionSpecification opt(SizeDecl) opt(DynamicActionSelectionDecl) "}"  { ActionProfileDecl $2 $4 $5 $6 }
| "action_selector" SelectorName "{" "selection_key" ":" FieldListCalcName ";" "}"  { ActionSelectorDecl $2 $6 }
| "table" TableName "{" opt(ReadFieldMatch) TableActions opt(MinSizeDecl) opt(MaxSizeDecl) opt(SizeDecl) opt(SupportTimeoutDecl) "}"  { TableDecl $2 $4 $5 $6 $7 $8 $9 }
| "control" ControlFunctionName ControlBlock  { ControlFunctionDecl $2 $3 }



ConstValue :: { ConstValue }
: opt(Sign) unsignedValue "'" unsignedValue  { ConstValue $1 (Just (toNumber $2)) (toNumber $4) }
| opt(Sign) unsignedValue                    { ConstValue $1 Nothing              (toNumber $2) }

Sign :: { Sign }
: "+" %prec Sign   { Positive }
| "-" %prec Sign  { Negative }

HeaderTypeName :: { HeaderTypeName }  : identifier  { tokenString $1 }
FieldName :: { FieldName }  : identifier  { tokenString $1 }
FieldValue :: { FieldValue }  : ConstValue  { $1 }
FieldListName :: { FieldListName }  : identifier  { tokenString $1 }
FieldListCalcName :: { FieldListCalcName }  : identifier  { tokenString $1 }
InstanceName :: { InstanceName }  : identifier  { tokenString $1 }
AlgorithmName :: { AlgorithmName }  : identifier  { tokenString $1 }
ValueSetName :: { ValueSetName }  : identifier  { tokenString $1 }
ParserStateName :: { ParserStateName }  : identifier  { tokenString $1 }
ControlFunctionName :: { ControlFunctionName }  : identifier  { tokenString $1 }
ParserExceptionName :: { ParserExceptionName }  : identifier  { tokenString $1 }
CounterName :: { CounterName }  : identifier  { tokenString $1 }
TableName :: { TableName }  : identifier  { tokenString $1 }
MeterName :: { MeterName }  : identifier  { tokenString $1 }
RegisterName :: { RegisterName }  : identifier  { tokenString $1 }
ActionName :: { ActionName }  : identifier  { tokenString $1 }
ParamName :: { ParamName }  : identifier  { tokenString $1 }
ActionProfileName :: { ActionProfileName }  : identifier  { tokenString $1 }
SelectorName :: { SelectorName }  : identifier  { tokenString $1 }
SEMAString :: { String }  : identifier  { tokenString $1 }

HeaderDecBody :: { HeaderDecBody }
: "fields" "{" list1(FieldDec) "}" MaybeLength MaybeMaxLength  { HeaderDecBody $3 $5 $6 }

FieldDec :: { FieldDec }
: FieldName ":" BitWidth SignedOrSaturatingExpr ";"  { FieldDec $1 $3 $4 }

BitWidth :: { BitWidth }
: "*"         { Variable }
| ConstValue  { BwValue $1 }

SignedOrSaturatingExpr :: { [SignedOrSaturating] }
:                                       { [] }
| "(" sep1(SignedOrSaturating,",") ")"  { $2 }

SignedOrSaturating :: { SignedOrSaturating }
: "signed"      { Signed }
| "saturating"  { Saturating }

MaybeLength :: { Maybe LengthExp }
:                             { Nothing }
| "length" ":" LengthExp ";"  { Just $3 }

LengthExp :: { LengthExp }
: ConstValue                { LeValue $1 }
| FieldName                 { LeFieldName $1 }
| LengthExp "*" LengthExp   { LeBinOp LeMul $1 $3 }
| LengthExp "+" LengthExp   { LeBinOp LeAdd $1 $3 }
| LengthExp "-" LengthExp   { LeBinOp LeSub $1 $3 }
| LengthExp "<<" LengthExp  { LeBinOp LeShiftL $1 $3 }
| LengthExp ">>" LengthExp  { LeBinOp LeShiftR $1 $3 }
| "(" LengthExp ")"         { $2 }

MaybeMaxLength :: { Maybe ConstValue }
:                                  { Nothing }
| "max_length" ":" ConstValue ";"  { Just $3 }

MaybeMetadataInitializer :: { Maybe [(FieldName, FieldValue)] }
:                         { Nothing }
| "{" list1(FieldNV) "}"  { Just $2 }

FieldNV :: { (FieldName, FieldValue) }
: FieldName ":" FieldValue ";"  { ($1, $3) }

FieldListEntry :: { FieldListEntry }
: FieldRef ";"       { FleFieldRef $1 }
| InstanceName "[" Index "]" ";"  { FleHeaderRef (HeaderRef $1 (Just $3)) }
| SEMAString ";"     { FleSEMA $1 }
| FieldValue ";"     { FleFieldValue $1 }
| "payload" ";"      { Payload }

FieldRef :: { FieldRef }
: HeaderRef "." FieldName  { FieldRef $1 $3 }

HeaderRef :: { HeaderRef }
: InstanceName MaybeIndex  { HeaderRef $1 $2 }

MaybeIndex :: { Maybe Index }
:                { Nothing }
| "[" Index "]"  { Just $2 }

Index :: { Index }
: ConstValue  { IValue $1 }
| "last"      { Last }

UpdateVerifySpec :: { UpdateVerifySpec }
: UpdateOrVerify FieldListCalcName opt(IfCond) ";"  { UpdateVerifySpec $1 $2 $3 }

UpdateOrVerify :: { UpdateOrVerify }
: "update"  { Update }
| "verify"  { Verify }

IfCond :: { IfCond }
: "if" "(" CalcBoolCond ")"  { $3 }

CalcBoolCond :: { IfCond }
: "valid" "(" IfCondValid ")"  { IfCondValid $3 }
| FieldRef "==" FieldValue       { IfCondEq $1 $3 }

IfCondValid :: { Either HeaderRef FieldRef }
: HeaderRef  { Left $1 }
| FieldRef   { Right $1 }

ParserFunctionBody :: { ParserFunctionBody }
: list(ExtractOrSetStmt) ReturnStmt  { ParserFunctionBody $1 $2 }

ExtractOrSetStmt :: { ExtractOrSetStmt }
: "extract" "(" HeaderExtractRef ")" ";"  { EosExtractStmt $3 }
| SetStmt                                 { EosSetStmt $1 }

SetStmt :: { SetStmt }
: "set_metadata" "(" FieldRef "," MetadataExpr ")" ";"  { SetStmt $3 $5 }

HeaderExtractRef :: { HeaderExtractRef }
: InstanceName MaybeHeaderExtractIndex  { HeaderExtractRef $1 $2 }

MaybeHeaderExtractIndex :: { Maybe HeaderExtractIndex }
:                             { Nothing }
| "[" HeaderExtractIndex "]"  { Just $2 }

HeaderExtractIndex :: { HeaderExtractIndex }
: ConstValue  { HeiValue $1 }
| "next"      { Next }

MetadataExpr :: { MetadataExpr }
: FieldValue      { MeFieldValue $1 }
| FieldOrDataRef  { MeFieldOrDataRef $1 }

ReturnStmt :: { ReturnStmt }
: ReturnValueType                                               { RsReturnValueType $1 }
| "return" "select" "(" sep1(FieldOrDataRef,",") ")" "{" list1(CaseEntry) "}"  { RsReturnSelect $4 $7 }

ReturnValueType :: { ReturnValueType }
: "return" SEMAString ";"                { RvtSEMA $2 }
| "parse_error" ParserExceptionName ";"  { RvtParserExcept $2 }

CaseEntry :: { CaseEntry }
: ValueList ":" CaseReturnValueType ";"  { CaseEntry $1 $3 }

ValueList :: { ValueList }
: sep1(ValueOrMasked,",")  { VlValueList $1 }
| "default"                { VlDefault }

CaseReturnValueType :: { ReturnValueType }
: SEMAString                         { RvtSEMA $1 }
| "parse_error" ParserExceptionName  { RvtParserExcept $2 }

ValueOrMasked :: { ValueOrMasked }
: FieldValue                    { VomFieldValue $1 }
| FieldValue "mask" FieldValue  { VomFieldValueMask $1 $3 }
| ValueSetName                  { VomValueSetName $1 }

FieldOrDataRef :: { FieldOrDataRef }
: FieldRef                                     { FodrFieldRef $1 }
| "latest" "." FieldName                       { FodrLatest $3 }
| "current" "(" ConstValue "," ConstValue ")"  { FodrCurrent $3 $5 }

ReturnOrDrop :: { ReturnOrDrop }
: "return" ControlFunctionName  { Return $2 }
| "parser_drop"                 { Drop }

BytesOrPackets :: { BytesOrPackets }
: "bytes"    { Bytes }
| "packets"  { Packets }

DirectOrStaticDecl :: { DirectOrStatic }
: "direct" ":" TableName ";"  { DirectAttrib $3 }
| "static" ":" TableName ";"  { StaticAttrib $3 }

InstanceCountDecl :: { ConstValue }
: "instance_count" ":" ConstValue ";"  { $3 }

MinWidthDecl :: { ConstValue }
: "min_width" ":" ConstValue ";"  { $3 }

SaturatingDecl :: { Bool }
: "saturating" ";"  { True }
|                   { False }

WidthOrLayoutDecl :: { WidthOrLayout }
: "width" ":" ConstValue ";"       { Width $3 }
| "layout" ":" HeaderTypeName ";"  { Layout $3 }

AttributeList :: { [SignedOrSaturating] }
:                                                    { [] }
| "attributes" ":" sep1(SignedOrSaturating,",") ";"  { $3 }

RegisterRef :: { RegisterRef }
: RegisterName "[" ConstValue "]" opt(snd(".",FieldName))  { RegisterRef $1 $3 $5 }

ParamList :: { [ParamName] }
:                      { [] }
| sep1(ParamName,",")  { $1 }

ActionHeader :: { ActionHeader }
: ActionName "(" ParamList ")"  { ActionHeader $1 $3 }

ActionStmt :: { ActionStmt }
: ActionName "(" ActionArgList ")" ";"  { ActionStmt $1 $3 }

ActionArgList :: { [ActionArg] }
:                      { [] }
| sep1(ActionArg,",")  { $1 }

ActionArg :: { ActionArg }
: FieldValue  { AaFieldValue $1 }
| FieldRef    { AaFieldRef $1 }
| InstanceName "[" Index "]"  { AaHeaderRef (HeaderRef $1 (Just $3)) }
| SEMAString  { AaSEMA $1 }

ActionSpecification :: { [ActionName] }
: "actions" "{" list1(fst(ActionName,";")) "}"  { $3 }

SizeDecl :: { ConstValue }
: "size" ":" ConstValue ";"  { $3 }

DynamicActionSelectionDecl :: { SelectorName }
: "dynamic_action_selection" ":" SelectorName ";"  { $3 }

ReadFieldMatch :: { [FieldMatch] }
: "reads" "{" list1(FieldMatch) "}"  { $3 }

FieldMatch :: { FieldMatch }
: FieldOrMaskedRef ":" FieldMatchType ";"  { FieldMatch $1 $3 }

FieldOrMaskedRef :: { FieldOrMaskedRef }
: HeaderRef                   { FomrHeaderRef $1 }
| FieldRef                    { FomrFieldRef $1 }
| FieldRef "mask" ConstValue  { FomrFieldRefMask $1 $3 }

FieldMatchType :: { FieldMatchType }
: "exact"    { Exact }
| "ternary"  { Ternary }
| "lpm"      { Lpm }
| "range"    { Range }
| "valid"    { Valid }

TableActions :: { TableActions }
: ActionSpecification                         { ActionSpecification $1 }
| "action_profile" ":" ActionProfileName ";"  { ActionProfileSpecification $3 }

MinSizeDecl :: { ConstValue }
: "min_size" ":" ConstValue ";"  { $3 }

MaxSizeDecl :: { ConstValue }
: "max_size" ":" ConstValue ";"  { $3 }

SupportTimeoutDecl :: { Bool }
: "support_timeout" ":" "true" ";"  { True }
| "support_timeout" ":" "false" ";"  { False }

ControlBlock :: { ControlBlock }
: "{" list(ControlStmt) "}"  { ControlBlock $2 }

ControlStmt :: { ControlStmt }
: "apply" "(" TableName ")" ";"                     { ApplyTableCall $3 }
| "apply" "(" TableName ")" "{" list(CaseList) "}"  { ApplyAndSelectBlock $3 $6 }
| IfElseStmt                                        { CsIfElseStmt $1 }
| ControlFunctionName "(" ")" ";"                   { CsControlFunctionName $1 }

CaseList :: { CaseList }
: ActionOrDefault ControlBlock  { ActionCase $1 $2 }
| HitOrMiss ControlBlock        { HitMissCase $1 $2 }

ActionOrDefault :: { ActionOrDefault }
: "default"   { AodDefault }
| ActionName  { AodActionName $1 }

HitOrMiss :: { HitOrMiss }
: "hit"   { Hit }
| "miss"  { Miss }

IfElseStmt :: { IfElseStmt }
: "if" "(" BoolExpr ")" ControlBlock opt(ElseBlock)  { IfElseStmt $3 $5 $6 }

ElseBlock :: { ElseBlock }
: "else" ControlBlock  { ElseControlBlock $2 }
| "else" IfElseStmt    { ElseIfElseStmt $2 }

BoolExpr :: { BoolExpr }
: "valid" "(" HeaderRef ")"  { BeValidHeaderRef $3 }
| "not" BoolExpr             { BeNot $2 }
| "(" BoolExpr ")"           { $2 }
| BoolExpr "and" BoolExpr    { BeBoolOp BoolAnd $1 $3 }
| BoolExpr "or" BoolExpr     { BeBoolOp BoolOr $1 $3 }
| Exp ">" Exp                { BeRelOp Gt $1 $3 }
| Exp ">=" Exp               { BeRelOp Gte $1 $3 }
| Exp "==" Exp               { BeRelOp Eq $1 $3 }
| Exp "<=" Exp               { BeRelOp Lte $1 $3 }
| Exp "<" Exp                { BeRelOp Lt $1 $3 }
| Exp "!=" Exp               { BeRelOp Neq $1 $3 }
| "true"                     { BeBool True }
| "false"                    { BeBool False }

Exp :: { Exp }
: Exp "+" Exp           { BinOp Add $1 $3 }
| Exp "*" Exp           { BinOp Mul $1 $3 }
| Exp "-" Exp           { BinOp Sub $1 $3 }
| Exp "<<" Exp          { BinOp ShiftL $1 $3 }
| Exp ">>" Exp          { BinOp ShiftR $1 $3 }
| Exp "&" Exp           { BinOp And $1 $3 }
| Exp "|" Exp           { BinOp Or $1 $3 }
| Exp "^" Exp           { BinOp Xor $1 $3 }
| "~" Exp               { UnOp UNot $2}
| "-" Exp %prec UMinus  { UnOp UNeg $2}
| FieldRef              { FRef $1 }
| ConstValueLight       { Value $1 }
| "(" Exp ")"           { $2 }

-- TODO: reduce/reduce conflict btwn ConstValue Sign and UNeg...
ConstValueLight :: { ConstValue }
: unsignedValue  { ConstValue Nothing Nothing (toNumber $1) }

-- Top-level parsers for testing partial parsing
ConstValueTop : list(ConstValue)  { $1 }


-- Util
fst(p,q)  : p q  { $1 }
snd(p,q)  : p q  { $2 }

opt(p)
: p  { Just $1 }
|    { Nothing }

rev_list1(p)
: p               { [$1] }
| rev_list1(p) p  { $2 : $1 }

list1(p)
: rev_list1(p)  { reverse $1 }

list(p)
: list1(p)  { $1 }
|           { [] }

sep1(p,q)
: p list(snd(q,p))  { $1 : $2 }

{
parseError :: [Token] -> Except String a
parseError tokens = case tokens of
  [] -> throwError "Parse error: no tokens left to parse."
  Token tok tokStr loc : _ ->
    throwError $ "Parse error: unexpected token " ++ show tok ++ " '" ++ tokStr ++ "' at " ++ show loc ++ "."

toNumber :: Token -> Integer
toNumber = number . filter (/= '_') . tokenString
  where
  number :: String -> Integer
  number a
    | isPrefixOf "0x" a = r a
    | isPrefixOf "0X" a = r a
    | isPrefixOf "0b" a = b a
    | isPrefixOf "0B" a = b a
    | all (flip elem ['0' .. '9']) a = r a
    | otherwise = error $ "Invalid number format: " ++ a
    where
      r = fromInteger . read
      b = foldr (\c s -> s*2 + c) 0 . reverse . map (toInteger . digitToInt) . drop 2
}
