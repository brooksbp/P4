module Language.P4.Parser.Tokens
  ( Token(..)
  , TokenName(..)
  , Position(..)
  , tokenString
  ) where

import Text.Printf

tokenString :: Token -> String
tokenString (Token _ s _) = s

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = printf "%s:%d:%d" f l c

data Token = Token TokenName String Position deriving (Show, Eq)

data TokenName
  = KW_action
  | KW_action_profile
  | KW_action_selector
  | KW_actions
  | KW_algorithm
  | KW_and
  | KW_apply
  | KW_attributes
  | KW_bytes
  | KW_calculated_field
  | KW_control
  | KW_counter
  | KW_current
  | KW_default
  | KW_direct
  | KW_dynamic_action_selection
  | KW_else
  | KW_exact
  | KW_extract
  | KW_false
  | KW_field_list
  | KW_field_list_calculation
  | KW_fields
  | KW_header
  | KW_header_type
  | KW_hit
  | KW_if
  | KW_input
  | KW_instance_count
  | KW_last
  | KW_latest
  | KW_layout
  | KW_length
  | KW_lpm
  | KW_mask
  | KW_max_length
  | KW_max_size
  | KW_metadata
  | KW_meter
  | KW_min_size
  | KW_min_width
  | KW_miss
  | KW_next
  | KW_not
  | KW_or
  | KW_output_width
  | KW_packets
  | KW_parse_error
  | KW_parser
  | KW_parser_drop
  | KW_parser_exception
  | KW_parser_value_set
  | KW_payload
  | KW_primitive_action
  | KW_range
  | KW_reads
  | KW_register
  | KW_result
  | KW_return
  | KW_saturating
  | KW_select
  | KW_selection_key
  | KW_set_metadata
  | KW_signed
  | KW_size
  | KW_static
  | KW_support_timeout
  | KW_table
  | KW_ternary
  | KW_true
  | KW_type
  | KW_update
  | KW_valid
  | KW_verify
  | KW_width
  | Identifier
  | Lit_unsigned
  | Sym_s_quote
  | Sym_paren_l
  | Sym_paren_r
  | Sym_bracket_l
  | Sym_bracket_r
  | Sym_brace_l
  | Sym_brace_r
  | Sym_colon
  | Sym_semi
  | Sym_comma
  | Sym_plus
  | Sym_dash
  | Sym_aster
  | Sym_lt
  | Sym_gt
  | Sym_dot
  | Sym_amp
  | Sym_bar
  | Sym_hat
  | Sym_tilde
  | Sym_eq
  | Sym_lt_lt
  | Sym_gt_gt
  | Sym_gt_eq
  | Sym_lt_eq
  | Sym_bang_eq
  | Unknown
  deriving (Show, Eq)
