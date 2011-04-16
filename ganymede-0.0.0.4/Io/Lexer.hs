{-# LANGUAGE NoMonomorphismRestriction #-}

module Io.Lexer where

import Io.Types (Pos)
import Io.Util ((><))

import Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Combinator as C
import Text.Parsec.Char (digit, oneOf)
import Text.Parsec.Prim

idCh :: [Char]
symCh :: [Char]

idCh = ['A'..'z']++symCh
-- "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz"
-- let idch = ['A'-'z' '=' '*' '/' '-' '+' '>' '<' '^' '_' '%' '@' '!' '?' '&' '|' '$' '\\' '~']

symCh = "!@#$%&*+/<=>?|-~"

--           oneOf "=*/-+><%!@?&|$~_"
ioLangDef :: LanguageDef st
ioLangDef = emptyDef
  { commentLine = "#!"
  , nestedComments = False
  , identStart = oneOf idCh
  , identLetter = digit <|> oneOf idCh
  , opStart = oneOf symCh
  , opLetter = oneOf idCh
  , reservedNames = ["declare","import","export","primitive", "variable"]
  , reservedOpNames = [".","->","↦","<<=","=>>","⇐","⇒",":",";"]
  }

-- The lexer
lexer      :: TokenParser u
lexer       = T.makeTokenParser ioLangDef    
      
ident          :: Parsec String u (Pos, String)
reserv'd       :: String -> Parsec String u ()
string         :: Parsec String u (Pos, String)
int            :: Parsec String u (Pos, Integer)
-- symbol         :: String -> Parsec String u String
lex'd          :: Parsec String u a -> Parsec String u a
whiteSpace     :: Parsec String u ()
parens         :: Parsec String u a -> Parsec String u a
curlies        :: Parsec String u a -> Parsec String u a
end            :: Parsec String u ()
lpar           :: Parsec String u ()
rpar           :: Parsec String u ()
semiC          :: Parsec String u ()
fColon         :: Parsec String u ()
period         :: Parsec String u ()

--getVar         :: Parsec String u Pos
lambda     :: Parsec String u Pos
putVar     :: Parsec String u Pos
getVar     :: Parsec String u Pos
declare    :: Parsec String u Pos
variable   :: Parsec String u Pos
primitive  :: Parsec String u Pos
modImp     :: Parsec String u Pos
modExport  :: Parsec String u Pos

lex'd       = T.lexeme lexer
parens      = T.parens lexer
curlies     = T.braces lexer
reserv'd    = T.reserved lexer
end         = C.eof
{-
  | "("                        { Llpar }
  | ")"                        { Lrpar }
  | ";"                        { Lsemi }
  | ":"                        { Lcolon }
  | "."                        { Lperiod }
  
  | "->"                       { Llambda  (pos lexbuf) }
  | "<<="                      { Lputvar  (pos lexbuf) }
  | "=>>"                      { Lgetvar  (pos lexbuf) }
  | "declare"                  { Ldeclare (pos lexbuf) }
  | "variable"                 { Lvariable (pos lexbuf) }
  | "primitive"                { Lprimitive (pos lexbuf) }
  | "import"                   { Limport (pos lexbuf) }
  | "export"                   { Lexport (pos lexbuf) }
-}
lpar        = reserv'd "("
rpar        = reserv'd ")"
semiC       = reserv'd ";"
fColon      = reserv'd ":"
period      = reserv'd "."
lambda      = (reserv'd "↦" <|> reserv'd "->") >> getPosition
putVar      = (reserv'd "⇐" <|> reserv'd "<<=") >> getPosition
getVar      = (reserv'd "⇒" <|> reserv'd "=>>") >> getPosition
declare     = reserv'd "declare" >> getPosition
variable    = reserv'd "variable" >> getPosition
primitive   = reserv'd "primitive" >> getPosition
modImp      = reserv'd "import" >> getPosition
modExport   = reserv'd "export" >> getPosition

{-
  | '-'? ['0'-'9']+            { Lint  (pos lexbuf,(int_of_string (Lexing.lexeme lexbuf))) }
  | '"' [^ '"']* '"'           { Lstring (pos lexbuf,string_chars (Lexing.lexeme lexbuf)) }
  | idch ( idch | ['0'-'9'] )* { Lident (pos lexbuf,(Lexing.lexeme lexbuf)) }
-}

int         = getPosition >< (C.notFollowedBy lambda >> T.integer lexer)
string      = getPosition >< T.stringLiteral lexer
ident       = getPosition >< T.identifier lexer
-- spaces
whiteSpace  = T.whiteSpace lexer

-- lex_source i = lexer i `catch` (\_ ->
--   throw (IoLexerError (fetch current_pos)))

{-

{
open Io_parser;;
open Io_types;;

let row = ref 1
let row_start = ref 1
let module_name = ref ""
let current_pos = ref (!module_name,!row,1)
let reset_pos = function mname ->
  module_name := mname;
  row := 1;
  row_start := 1
  
let pos = function lexbuf ->
  current_pos := (!module_name, !row, (Lexing.lexeme_start lexbuf) - (!row_start));
  !current_pos
  
let string_chars s = String.sub s 1 ((String.length s)-2) ;;
  
}
let idch = ['A'-'z' '=' '*' '/' '-' '+' '>' '<' '^' '_' '%' '@' '!' '?' '&' '|' '$' '\\' '~']
rule lexer = parse
  [' ' '\t' '\r']              { lexer lexbuf }
  | '\n'                       { row := (!row) + 1;row_start := Lexing.lexeme_start lexbuf;lexer lexbuf }
  | '#' [^ '\n']* '\n'         { row := (!row) + 1;row_start := Lexing.lexeme_start lexbuf;lexer lexbuf }
  | eof                        { Lend }
  | "("                        { Llpar }
  | ")"                        { Lrpar }
  | ";"                        { Lsemi }
  | ":"                        { Lcolon }
  | "."                        { Lperiod }
  | "->"                       { Llambda  (pos lexbuf) }
  | "<<="                      { Lputvar  (pos lexbuf) }
  | "=>>"                      { Lgetvar  (pos lexbuf) }
  | "declare"                  { Ldeclare (pos lexbuf) }
  | "variable"                 { Lvariable (pos lexbuf) }
  | "primitive"                { Lprimitive (pos lexbuf) }
  | "import"                   { Limport (pos lexbuf) }
  | "export"                   { Lexport (pos lexbuf) }
  | '-'? ['0'-'9']+            { Lint  (pos lexbuf,(int_of_string (Lexing.lexeme lexbuf))) }
  | '"' [^ '"']* '"'           { Lstring (pos lexbuf,string_chars (Lexing.lexeme lexbuf)) }
  | idch ( idch | ['0'-'9'] )* { Lident (pos lexbuf,(Lexing.lexeme lexbuf)) }

{

let lex_source =
function i ->
  try
    lexer i
  with
    Failure e ->
      raise (Io_module_error (Io_lexer_error !current_pos))
}
 -}

