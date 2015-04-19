-- -*-haskell-*-
{
module Language.P4.Parser.Lex
  ( alexScanTokens
  ) where

import Language.P4.Parser.Tokens
}

%wrapper "posn"

$binit = [0-1]
$decit = [$binit 2-9]
$hexit = [$decit A-F a-f]

@bin = $binit [$binit \_]*
@dec = $decit [$decit \_]*
@hex = $hexit [$hexit \_]*

@binNum = 0[bB] @bin
@decNum =       @dec
@hexNum = 0[xX] @hex

@unsignedValue = @decNum | @binNum | @hexNum

@identifier = [a-zA-Z_] [a-zA-Z0-9_]*


tokens :-

  "action"            { tok KW_action }
  "action_profile"    { tok KW_action_profile }
  "action_selector"   { tok KW_action_selector }
  "actions"           { tok KW_actions }
  "algorithm"         { tok KW_algorithm }
  "and"               { tok KW_and }
  "apply"             { tok KW_apply }
  "attributes"        { tok KW_attributes }
  "bytes"             { tok KW_bytes }
  "calculated_field"  { tok KW_calculated_field }
  "control"           { tok KW_control }
  "counter"           { tok KW_counter }
  "current"           { tok KW_current }
  "default"           { tok KW_default }
  "direct"            { tok KW_direct }
  "dynamic_action_selection" { tok KW_dynamic_action_selection }
  "else"              { tok KW_else }
  "exact"             { tok KW_exact }
  "extract"           { tok KW_extract }
  "false"             { tok KW_false }
  "field_list"        { tok KW_field_list }
  "field_list_calculation" { tok KW_field_list_calculation }
  "fields"            { tok KW_fields }
  "header"            { tok KW_header }
  "header_type"       { tok KW_header_type }
  "hit"               { tok KW_hit }
  "if"                { tok KW_if }
  "input"             { tok KW_input }
  "instance_count"    { tok KW_instance_count }
  "last"              { tok KW_last }
  "latest"            { tok KW_latest }
  "layout"            { tok KW_layout }
  "length"            { tok KW_length }
  "lpm"               { tok KW_lpm }
  "mask"              { tok KW_mask }
  "max_length"        { tok KW_max_length }
  "max_size"          { tok KW_max_size }
  "metadata"          { tok KW_metadata }
  "meter"             { tok KW_meter }
  "min_size"          { tok KW_min_size }
  "min_width"         { tok KW_min_width }
  "miss"              { tok KW_miss }
  "next"              { tok KW_next }
  "not"               { tok KW_not }
  "or"                { tok KW_or }
  "output_width"      { tok KW_output_width }
  "packets"           { tok KW_packets }
  "parse_error"       { tok KW_parse_error }
  "parser"            { tok KW_parser }
  "parser_drop"       { tok KW_parser_drop }
  "parser_exception"  { tok KW_parser_exception }
  "parser_value_set"  { tok KW_parser_value_set }
  "payload"           { tok KW_payload }
  "primitive_action"  { tok KW_primitive_action }
  "range"             { tok KW_range }
  "reads"             { tok KW_reads }
  "register"          { tok KW_register }
  "result"            { tok KW_result }
  "return"            { tok KW_return }
  "saturating"        { tok KW_saturating }
  "select"            { tok KW_select }
  "selection_key"     { tok KW_selection_key }
  "set_metadata"      { tok KW_set_metadata }
  "signed"            { tok KW_signed }
  "size"              { tok KW_size }
  "static"            { tok KW_static }
  "support_timeout"   { tok KW_support_timeout }
  "table"             { tok KW_table }
  "ternary"           { tok KW_ternary }
  "true"              { tok KW_true }
  "type"              { tok KW_type }
  "update"            { tok KW_update }
  "valid"             { tok KW_valid }
  "verify"            { tok KW_verify }
  "width"             { tok KW_width }

  @identifier         { tok Identifier }

  @unsignedValue      { tok Lit_unsigned }

  "'" { tok Sym_s_quote }
  "(" { tok Sym_paren_l }
  ")" { tok Sym_paren_r }
  "[" { tok Sym_bracket_l }
  "]" { tok Sym_bracket_r }
  "{" { tok Sym_brace_l }
  "}" { tok Sym_brace_r }
  ":" { tok Sym_colon }
  ";" { tok Sym_semi }
  "," { tok Sym_comma }
  "+" { tok Sym_plus }
  "-" { tok Sym_dash }
  "*" { tok Sym_aster }
  "<" { tok Sym_lt }
  ">" { tok Sym_gt }
  "." { tok Sym_dot }
  "&" { tok Sym_amp }
  "|" { tok Sym_bar }
  "^" { tok Sym_hat }
  "~" { tok Sym_tilde }

  "==" { tok Sym_eq }
  "<<" { tok Sym_lt_lt }
  ">>" { tok Sym_gt_gt }
  ">=" { tok Sym_gt_eq }
  "<=" { tok Sym_lt_eq }
  "!=" { tok Sym_bang_eq }

  $white ;

  . { tok Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c
}
