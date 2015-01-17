import System.Environment (getArgs)
import Control.Applicative
import Control.Lens

data Cmd = Add Int
         | Sub Int
         | RightS Int
         | LeftS Int
         | In
         | Out
         | Loop [Cmd]
         | Clear
         deriving (Show, Eq)

main = head <$> getArgs >>= readFile >>= print . optimize . parse

parse :: [Char] -> [Cmd]
parse [] = []
parse ('+':xs) = Add 1     : parse xs
parse ('-':xs) = Sub 1     : parse xs
parse ('>':xs) = RightS 1  : parse xs
parse ('<':xs) = LeftS 1   : parse xs
parse ('.':xs) = Out       : parse xs
parse (',':xs) = In        : parse xs
parse ('[':xs) = Loop body : parse remaining
 where (body, remaining) = over _1 parse $ break (== ']') xs
parse (_:xs) = parse xs

optimize = contract . clear

clear = map (\(Loop [Sub 1]) -> Clear)

contract [] = []
contract (x:cs) = case x of
  Out    -> x : contract cs
  In     -> x : contract cs
  Loop b -> Loop (contract b) : contract cs
  _      -> contract' x cs

contract' first cs = instr reps : contract rest
  where (same, rest) = break (/= first) cs
        reps = length same + 1
        instr = case first of
                  Add    _ -> Add
                  Sub    _ -> Sub
                  RightS _ -> RightS
                  LeftS  _ -> LeftS

compile :: [Cmd] -> [String]
compile cs = header : map convert cs ++ [footer]
 where convert (Add n) = "mem[p] +=" ++ show n ++ ";\n"
       convert (Sub n) = "mem[p] -= " ++ show n ++ ";\n"
       convert (RightS n) = "p += " ++ show n ++ ";\n"
       convert (LeftS n) = "p -= " ++ show n ++ ";\n"
       convert Out = "printf(\"%c\", mem[p]);"
       convert In = "scanf(\"%c\", &mem[p]);"
       convert (Loop b) = "while(mem[p]){\n" ++ concatMap convert b ++ "}\n"
       header = "#include <stdio.h>\nint main(void){\nchar mem[65536] = {0};int p = 0;"
       footer = "return 0; }"
