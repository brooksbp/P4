module Language.P4.Sema.Header
  (
  ) where

import Data.Bits
import Data.Maybe

import Language.P4.AST

numVariableLengthFields :: HeaderDecBody -> Int
numVariableLengthFields (HeaderDecBody fx _ _) = length $ filter match fx
  where
    match :: FieldDec -> Bool
    match (FieldDec _ Variable _) = True
    match _ = False

sumOfFixedWidthFields :: HeaderDecBody -> Integer
sumOfFixedWidthFields (HeaderDecBody fx _ _) = sum $ map e fx
  where
    e :: FieldDec -> Integer
    e (FieldDec _ (BwValue val) _) = c2i val
    e _ = 0

evalLengthExp :: LengthExp -> [(FieldName, Integer)] -> Integer
evalLengthExp e fx = evalLE e
  where
    evalLE (LeValue val) = c2i val
    evalLE (LeFieldName name) = fromMaybe 0 (lookup name fx)
    evalLE (LeBinOp op e1 e2) =
      let e1' = evalLE e1
          e2' = evalLE e2 in case op of
                              LeAdd -> e1' + e2'
                              LeSub -> e1' - e2'
                              LeMul -> e1' * e2'
                              LeShiftL -> e1' `shiftL` fromIntegral e2'
                              LeShiftR -> e1' `shiftR` fromIntegral e2'

-- One field at most within a header type may specify a widht of "*" which
-- indicates it is of variable length.

-- "fixed length" - all fields are fixed-width (no "*"), otherwise "variable len"

-- "length" attribute: length of header in bytes for variable length headers
--    - must be present if "variable len"
--    - warn if present for "fixed length"

-- "max_length" attribute: max length of header in bytes for vlh
--    - must be present if "variable len"
--    - warn if present for "fixed length"

-- width of "*" field is inferred: (8 * length) - sum-of-fixed-width-fields
