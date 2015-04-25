module Main where

import qualified Language.P4 as P4

import CodeGen

import Options.Applicative

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

compile :: FilePath -> IO ()
compile file = do
  contents <- readFile file
  case parseAndFixup file contents of
   Left err -> print err
   Right ast -> writeFile "softswitch.c" (ppCgen ast)

data Cmd
  = AstDump FilePath
  | Compile FilePath
  deriving (Show)

cmd :: Parser Cmd
cmd = subparser
      ( command "ast-dump" (info cmdAstDump (progDesc "Parse and dump AST."))
        <>
        command "compile" (info cmdCompile (progDesc "Compile P4 program."))
      )

cmdAstDump :: Parser Cmd
cmdAstDump = AstDump <$> argument str (metavar "PROGRAM")

cmdCompile :: Parser Cmd
cmdCompile = Compile <$> argument str (metavar "PROGRAM")

run :: Cmd -> IO ()
run (AstDump file) = astDump file
run (Compile file) = compile file

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (cmd <**> helper) idm
