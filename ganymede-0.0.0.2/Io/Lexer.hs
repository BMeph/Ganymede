{-# LANGUAGE NoMonomorphismRestriction #-}

module Io.Lexer where

import Io.Util
import Io.Types

import Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef, LanguageDef (..))
import Text.Parsec.Combinator as C
import Text.Parsec.Char (digit, oneOf, spaces)
import Text.Parsec.Prim
-- import Text.Parsec.String

-- type IoParser a = GenParser IoToken () a

{-
row = ref 1
row_start = ref 1
module_name = ref ""
current_pos = ref (fetch module_name, fetch row, 1)
reset_pos mname = do {
  module_name <-< mname;
  row <-< 1;
  row_start <-< 1}
  
pos lexbuf = do {
  current_pos <-< fmap (,,) <*> readIORef module_name <*>
      readIORef row <*> (Lexing.lexeme_start lexbuf) - (readIORef row_start)
  readIORef current_pos}
  
-- string_chars: Used to chop off the bracketing double-quotes from strings;
-- (@faq Can Haskell just parse away the quotes instead?)
-- string_chars s  = (drop 2 . init . (' ':)) s
 -}

-- Lexer token type aliases
data IoToken =
    LInt Int
 |  LString String
 |  LIdent String
 |  LSemi
 |  LLPar
 |  LRPar
 |  LColon
 |  LPeriod
 |  LLambda
 |  LEnd
 |  LDeclare
 |  LPrimitive
 |  LVariable
 |  LExport
 |  LImport
 |  ParseIoModule IoAST

type IoLex = (Pos, IoToken)



idCh = ['A'..'z']++"=*/-+><%@!?&|$~"

ioLangDef :: LanguageDef st
ioLangDef = emptyDef
  { commentLine = "#!"
  , nestedComments = False
  , identStart = oneOf idCh
  , identLetter = digit <|> oneOf idCh
  , opLetter = parserZero  -- Io has no operators
  , reservedNames = ["declare","import","export","primitive",
                     "variable",".","->","<<=","=>>",":",";"]
  }

-- The lexer
lexer       = T.makeTokenParser ioLangDef    
      
parens      = T.parens lexer
curlies     = T.braces lexer
ident       = T.identifier lexer
reserv'd    = T.reserved lexer
end         = C.eof
lpar        = reserv'd "("
rpar        = reserv'd ")"
semiC       = T.semi lexer
fColon      = T.colon lexer
period      = T.dot lexer
lambda      = reserv'd "->" >> getPosition
int         = T.integer lexer
string      = T.stringLiteral lexer
putVar      = reserv'd "<<=" >> getPosition
getVar      = reserv'd "=>>" >> getPosition
declare     = reserv'd "declare" >> getPosition
variable    = reserv'd "variable" >> getPosition
primitive   = reserv'd "primitive" >> getPosition
modImp      = reserv'd "import" >> getPosition
modExport   = reserv'd "export" >> getPosition
lex'd       = T.lexeme lexer
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

