module Language.P4.Parser
  ( parseProgram
  , parseConstValue
  ) where

import Control.Monad.Except
import Language.P4.AST
import Language.P4.Parser.Lex
import Language.P4.Parser.Parse
import Language.P4.Parser.Preprocess
import Language.P4.Parser.Tokens

parse :: ([Token] -> Except e a) -> FilePath -> String -> Either e a
parse parser file input =
  let tokenStream = map sprinkle $ alexScanTokens $ preprocess [] file input in
  runExcept (parser tokenStream)
  where
    sprinkle :: Token -> Token
    sprinkle (Token t s (Position _ l c)) = Token t s $ Position file l c

parseProgram :: FilePath -> String -> Either String Program
parseProgram = parse program

parseConstValue :: FilePath -> String -> Either String [ConstValue]
parseConstValue = parse constValue
