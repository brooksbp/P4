module Main where

import qualified Language.P4 as P4
import Options.Applicative
import System.IO

type Target = String


parseAndFixup :: FilePath -> String -> Either String P4.Program
parseAndFixup fileName input = do
  ast <- P4.parseProgram fileName input
  P4.fixupSEMAs ast

astDump :: FilePath -> IO ()
astDump file = do
  contents <- readFile file
  case parseAndFixup file contents of
   Left err -> print err
   Right ast -> print ast


data Cmd
  = AstDump FilePath
  | Compile Target FilePath
  deriving (Show)

cmd :: Parser Cmd
cmd = subparser
      ( command "ast-dump" (info cmdAstDump (progDesc "Parse and dump AST."))
        <>
        command "compile"  (info cmdCompile (progDesc "Compile program."))
      )

cmdAstDump :: Parser Cmd
cmdAstDump = AstDump <$> argument str (metavar "PROGRAM")

cmdCompile :: Parser Cmd
cmdCompile = error "TODO"

run :: Cmd -> IO ()
run (AstDump file) = astDump file
run _ = error "TODO"

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (cmd <**> helper) idm
