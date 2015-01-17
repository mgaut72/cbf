module Language.BrainFuck.Compile
  ( compile
  ) where

import Text.Printf

import Language.BrainFuck.AST

compile :: [Cmd] -> String
compile cs = printf "%s\n%s\n%s" header (body cs) footer

header :: String
header = printf "%s\n%s\n%s" incl mn tape
 where incl = "#include <stdio.h>"
       mn   = "int main(void){"
       tape = "\tchar mem[65536] = {0};\n\tint p = 0;"

footer :: String
footer = "\treturn 0;\n}"

body :: [Cmd] -> String
body = indent . concatMap convert

convert :: Cmd -> String
convert Add      = "mem[p]++\n"
convert Sub      = "mem[p]--\n"
convert RightS   = "p++\n"
convert LeftS    = "p--\n"
convert Out      = "printf(\"%c\", mem[p]);\n"
convert In       = "scanf(\"%c\", &mem[p]);\n"
convert (Loop b) = "while(mem[p]){\n" ++ indent (concatMap convert b) ++ "}\n"

indent = unlines . map ('\t':) . lines
