module Language.P4.Parser.Preprocess
  ( preprocess
  ) where

import Data.List (isPrefixOf)

stripComments :: FilePath -> String -> String
stripComments file = uncomment
  where
    uncomment a = case a of
      ""               -> ""
      '/' : '/' : rest -> "  " ++ removeEOL rest
      '/' : '*' : rest -> "  " ++ remove rest
      '"'       : rest -> '"' : ignoreString rest
      a         : rest -> a   : uncomment rest

    removeEOL a = case a of
      ""          -> ""
      '\n' : rest -> '\n' : uncomment rest
      '\t' : rest -> '\t' : removeEOL rest
      _    : rest -> ' '  : removeEOL rest

    remove a = case a of
      ""               -> error $ "File ended without closing comment (*/): " ++ file
      '"'  : rest      -> removeString rest
      '\n' : rest      -> '\n' : remove rest
      '\t' : rest      -> '\t' : remove rest
      '*' : '/' : rest -> "  " ++ uncomment rest
      _ : rest         -> ' ' : remove rest

    removeString a = case a of
      ""                -> error $ "File ended without closing string: " ++ file
      '"' : rest        -> " " ++  remove rest
      '\\' : '"' : rest -> "  " ++ removeString rest
      '\n' : rest       -> '\n' :  removeString rest
      '\t' : rest       -> '\t' :  removeString rest
      _    : rest       -> ' '  :  removeString rest

    ignoreString a = case a of
      ""                -> error $ "File ended without closing string: " ++ file
      '"' : rest        -> '"' : uncomment rest
      '\\' : '"' : rest -> "\\\"" ++ ignoreString rest
      a : rest          -> a : ignoreString rest

preprocess :: [(String, String)] -> FilePath -> String -> String
preprocess env file content = unlines $ pp True [] env $ lines $ stripComments file content
  where
    pp :: Bool -> [Bool] -> [(String, String)] -> [String] -> [String]
    pp _ _ _ [] = []
    pp on stack env (a : rest) = case words a of
      "#define" : name : value -> "" : pp on stack (if on then (name, ppLine env $ unwords value) : env else env) rest
      _ -> (if on then ppLine env a else "") : pp on stack env rest

ppLine :: [(String, String)] -> String -> String
ppLine ((name, value) : xs) s = ppLine xs (rep name value s)
ppLine [] s = s

rep :: Eq a => [a] -> [a] -> [a] -> [a]
rep a b s@(x:xs) =
  if a `isPrefixOf` s
  then b ++ rep a b (drop (length a) s)
  else x : rep a b xs
