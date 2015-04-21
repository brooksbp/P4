module Language.P4.Sema
  ( -- * Namespace fixups
    fixupFleSEMAs
  , fixupRvtSEMAs
  , fixupAaSEMAs
  ) where

import Control.Monad
import Control.Monad.Except

import Language.P4.AST
import Language.P4.Walk


fixupFleSEMAs :: Program -> Except String Program
fixupFleSEMAs p = foldM fixupFleSEMA p (extractFleSEMAs p)

fixupFleSEMA :: Program -> String -> Except String Program
fixupFleSEMA p s
  | (s `elem` ix) && (s `elem` fx) = throwError $ "FleSEMA '" ++ s ++ "' ns collision."
  | s `elem` ix = return $ walk (g1 s) p
  | s `elem` fx = return $ walk (g2 s) p
  | otherwise = throwError $ "Cannot resolve FleSEMA '" ++ s ++ "'."
  where
    ix = extractInstanceNames p
    fx = extractFieldListNames p

    g1 :: String -> FieldListEntry -> FieldListEntry
    g1 r fle = case fle of
      (FleSEMA r') | r' == r -> FleHeaderRef $ HeaderRef r Nothing
      _                      -> fle
    g2 :: String -> FieldListEntry -> FieldListEntry
    g2 r fle = case fle of
      (FleSEMA r') | r' == r -> FleFieldListName r
      _                      -> fle

extractFieldListEntry :: Decl -> [FieldListEntry]
extractFieldListEntry (FieldListDecl _ xs) = xs
extractFieldListEntry _ = []

extractFleSEMA :: FieldListEntry -> [String]
extractFleSEMA (FleSEMA s) = [s]
extractFleSEMA _ = []

extractFleSEMAs :: Program -> [String]
extractFleSEMAs = query extractFleSEMA . query extractFieldListEntry

extractInstanceNames :: Program -> [InstanceName]
extractInstanceNames = query e
  where
    e :: Decl -> [InstanceName]
    e (ScalarInstanceDecl _ n) = [n]
    e (ArrayInstanceDecl _ n _) = [n]
    e (MetadataInstanceDecl _ n _) = [n]
    e _ = []

extractFieldListNames :: Program -> [FieldListName]
extractFieldListNames = query e
  where
    e :: Decl -> [FieldListName]
    e (FieldListDecl n _) = [n]
    e _ = []


fixupRvtSEMAs :: Program -> Except String Program
fixupRvtSEMAs p = foldM fixupRvtSEMA p (extractRvtSEMAs p)

fixupRvtSEMA :: Program -> String -> Except String Program
fixupRvtSEMA p s
  | (s `elem` px) && (s `elem` cx) = throwError $ "RvtSEMA '" ++ s ++ "' ns collision."
  | s `elem` px = return $ walk (g1 s) p
  | s `elem` cx = return $ walk (g2 s) p
  | otherwise = throwError $ "Cannot resolve RvtSEMA '" ++ s ++ "'."
  where
    px = extractParserStateNames p
    cx = extractControlFunctionNames p

    g1 :: String -> ReturnValueType -> ReturnValueType
    g1 r rvt = case rvt of
      (RvtSEMA r') | r' == r -> RvtParserState r
      _                      -> rvt
    g2 :: String -> ReturnValueType -> ReturnValueType
    g2 r rvt = case rvt of
      (RvtSEMA r') | r' == r -> RvtControlFunc r
      _                      -> rvt

extractReturnStmt :: Decl -> [ReturnStmt]
extractReturnStmt (ParserFunctionDecl _ (ParserFunctionBody _ rx)) = [rx]
extractReturnStmt _ = []

extractReturnValueType :: ReturnStmt -> [ReturnValueType]
extractReturnValueType (RsReturnValueType r) = [r]
extractReturnValueType (RsReturnSelect _ cx) = map (\(CaseEntry _ r) -> r) cx

extractRvtSEMA :: ReturnValueType -> [String]
extractRvtSEMA (RvtSEMA s) = [s]
extractRvtSEMA _ = []

extractRvtSEMAs :: Program -> [String]
extractRvtSEMAs = query extractRvtSEMA . query extractReturnValueType . query extractReturnStmt

extractParserStateName :: Decl -> [ParserStateName]
extractParserStateName (ParserFunctionDecl n _) = [n]
extractParserStateName _ = []

extractParserStateNames :: Program -> [ParserStateName]
extractParserStateNames = query extractParserStateName

extractControlFunctionName :: Decl -> [ControlFunctionName]
extractControlFunctionName (ControlFunctionDecl n _)  = [n]
extractControlFunctionName _ = []

extractControlFunctionNames :: Program -> [ControlFunctionName]
extractControlFunctionNames = query extractControlFunctionName


fixupAaSEMAs :: Program -> Except String Program
fixupAaSEMAs p = foldM fixupAaSEMA p (extractAaSEMAs p)

fixupAaSEMA :: Program -> String -> Except String Program
fixupAaSEMA p s
  | s `elem` ix = return $ walk (g1 s) p
  | otherwise   = return $ walk (g2 s) p
  where
    ix = extractInstanceNames p

    g1 :: String -> ActionArg -> ActionArg
    g1 r aa = case aa of
      (AaSEMA r') | r' == r -> AaHeaderRef $ HeaderRef r Nothing
      _                     -> aa
    g2 :: String -> ActionArg -> ActionArg
    g2 r aa = case aa of
      (AaSEMA r') | r' == r -> AaParamName r
      _                     -> aa

extractActionArgs :: Decl -> [ActionArg]
extractActionArgs (ActionFunctionDecl _ ax) = concatMap (\(ActionStmt _ aax) -> aax) ax
extractActionArgs _ = []

extractAaSEMA :: ActionArg -> [String]
extractAaSEMA (AaSEMA s) = [s]
extractAaSEMA _ = []

extractAaSEMAs :: Program -> [String]
extractAaSEMAs = query extractAaSEMA . query extractActionArgs
