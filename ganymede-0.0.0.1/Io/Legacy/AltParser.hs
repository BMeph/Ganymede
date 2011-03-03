{-#  #-}
-- module Io.Parser where
module Io.Parser where

module Io.Parser ( Parser
                 , parse_error
                 , parse_io_module
                 , parseFromFile
                 ) where

import Io.Types
import Io.Lex as Lex
import Io.Messages
import Io.Util (freshPos, initialPos)

import Control.Exception (throw, throwIO)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Token

import Control.Exception (throw, throwIO)
import Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef, LanguageDef (..), token)
import Text.Parsec.Combinator as C
import Text.Parsec.Char (digit, oneOf, spaces)
import Text.Parsec.Prim (parse)

ganyScanner :: String -> ([IoLex],[String])
-- ^ ganyScanner: Takes a string and tokenizes it; returning a list of Io tokens (IoToken)
--     and a list of error message strings (if any).
ganyScanner = undefined

-- The lexer
lexer       = T.makeTokenParser ioLangDef    
      
parens      = T.parens lexer
braces      = T.braces lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
end         = C.eof
lpar        = (T.reserved "(") lexer
rpar        = (T.reserved ")") lexer
semi        = T.semi lexer
colon       = T.colon lexer
period      = T.dot lexer
lambda      = (T.reserved "->") lexer
int         = T.integer lexer
string      = T.stringLiteral lexer
putVar      = (T.reserved "<<=") lexer
getVar      = (T.reserved "=>>") lexer
lexeme      = T.lexeme lexer
whiteSpace  = T.whiteSpace lexer
-- spaces

ioParse :: Show a => Parser a -> String -> IO ()
ioParse ioParser input = case (parse p "" input) of
    Left err -> do
      { putStr "parse error at "
      ; print err
      }
    Right x -> print x

ioLangDef :: LanguageDef st
ioLangDef = emptyDef
  { commentLine = "# "
  , nestedComments = False
  , identStart = oneOf Lex.idCh
  , identLetter = digit <|> oneOf Lex.idCh
  , opLetter = pzero  -- Io has no operators
  , reservedNames = ["import","export","primitive","variable",".","->","<<=","=>>",":",";"]
  }


lastPos :: Pos
lastPos = freshPos
-- last_pos = IoParserState "" 0 0

resetPos :: String -> Pos  -> Pos 
resetPos mname = \_ -> (initialPos mname)

pos = fst
unpos = snd
ct1 c (cs, bs) = (c:cs, bs)
ct2 b (cs, bs) = (cs, b:bs)
parse_error last_pos _  = throw (IoParserError last_pos)

-- start_parser = ParseIoModule ([],[],([],[]), EPrimitive "")

{-
parse_io_module:
  | imports export decll expr Lend             { ($1,$2,$3,$4) }
 -}

parse_io_module :: Parser IoAST
parse_io_module = do
  { is <- parse parse_imports
  ; es <- parse parse_exports
  ; dPair <-  parse parse_decls
  ; ps <- parse parse_exprs
  ; end
  ; return (is,es,dPair,ps)
  }

{-
imports:
  | Limport Lident importlist Lperiod imports  { last_pos := pos $2;(String.lowercase (unpos $2), $3)::$5 }
  |                                            { [] }

importlist:
  | Lcolon idlist                              { Some $2 }
  |                                            { None }
-}
parse_imports :: Parser IoImports
parse_imports = many do
 { reserved "import"
 ; mName <- identifier
 ; imps <- impList
 ; period
 ; return (mName, imps)
 } <?> "import statement" where
  impList = optionMaybe (colon >> idList)


{-
export:
  | Lexport idlist Lperiod                     { $2 }
  |                                            { [] }
-}

parse_exports :: Parser IoExports
parse_exports = option [] (
  between (reserved "export") period idList) <?> "export statement"

{-
 decll:
  | Ldeclare Lident Lcolon eatom Lperiod decll    { last_pos := pos $2;ct1 ($1,unpos $2,$4) $6 }
  | Lprimitive Lident Lcolon Lident Lperiod decll { last_pos := pos $2;ct1 ($1,unpos $2,Eprimitive (unpos $4)) $6 }
  | Lvariable Lident Lcolon eatom Lperiod decll   { last_pos := pos $2;ct2 ($1,unpos $2,$4) $6 }
  |                                               { ([], []) }
 -}
  
parse_decls :: Parser ([IoDecl], [IoDecl])
parse_decls = undefined <?> "declaration"{--}

{-
expr:
  | Lident paramlist                           { Eappl (pos $1, unpos $1, $2, EFall) }
  | Lident                                     { Eid (pos $1, unpos $1) }
  | Lident Lputvar patom stmttail              { Eputvar (pos $1, unpos $1, $3, $4, EFall) } 
  | Lident Lgetvar Lident stmttail             { Egetvar (pos $1, unpos $1, unpos $3, $4, EFall) } 
  | stmt                                       { $1 }
 -}
 
parse_exprs :: Parser IoExpr
parse_exprs = try stmt <|> undefined <?> "expression"{--}

{-
idlist:
  | Lident idlist                              { last_pos := pos $1;(unpos $1)::$2 }
  |                                            { [] }
-}

idList :: Parser [IoID]
idList = many identifier

{-
paramlist:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi eatom                                { [$2]   }
  | stmt                                       { [$1] }

{--}
  
paramlisttail:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi eatom                                { [$2]   }
  | stmt                                       { [$1] }
  |                                            { [] }

{--}
  
stmt:
  | Llambda idlist stmttail                    { Elambda ($1,$2,$3,EFall) }
  
-}
stmt :: Parser IoExpr =
       ELambda Pos [IoID] IoExpr EnvFilter
stmt = do
  { reservedOp "->"
  ;
  ;
  ;
  ;
  ;
  ;
  ; return (ELambda p is ioex eFil)
  } <?> "statement"
  
{-  
stmttail:
  | Lsemi eatom                                { $2 }
  | atom                                       { $1 }
 -} 

stTail :: Parsec
stTail = (try semi >> eAtom) <|> atom
  
{-
%{
  open Io_types ;;
  open Io_messages ;;
  open Io_parser_state ;;
  let unpos = snd
  let pos = fst
  let ct1 = fun c (cs, bs) -> (c::cs, bs)
  let ct2 = fun b (cs, bs) -> (cs, b::bs)
  let parse_error = function msg ->
    raise (Io_module_error (Io_parser_error !last_pos))

%}

%token <Io_types.pos * int> Lint
%token <Io_types.pos * string> Lstring
%token <Io_types.pos * string> Lident
%token Llpar Lrpar Lsemi
%token Lcolon Lperiod
%token <Io_types.pos> Llambda
%token <Io_types.pos> Lputvar
%token <Io_types.pos> Lgetvar
%token Lend
%token <Io_types.pos> Ldeclare
%token <Io_types.pos> Lprimitive
%token <Io_types.pos> Lvariable
%token <Io_types.pos> Lexport
%token <Io_types.pos> Limport

%start parse_io_module
%type <Io_types.io_ast> parse_io_module

%%

parse_io_module:
  | imports export decll expr Lend             { ($1,$2,$3,$4) }

imports:
  | Limport Lident importlist Lperiod imports  { last_pos := pos $2;(String.lowercase (unpos $2), $3)::$5 }
  |                                            { [] }

importlist:
  | Lcolon idlist                              { Some $2 }
  |                                            { None }

export:
  | Lexport idlist Lperiod                     { $2 }
  |                                            { [] }

decll:
  | Ldeclare Lident Lcolon eatom Lperiod decll    { last_pos := pos $2;ct1 ($1,unpos $2,$4) $6 }
  | Lprimitive Lident Lcolon Lident Lperiod decll { last_pos := pos $2;ct1 ($1,unpos $2,Eprimitive (unpos $4)) $6 }
  | Lvariable Lident Lcolon eatom Lperiod decll   { last_pos := pos $2;ct2 ($1,unpos $2,$4) $6 }
  |                                               { ([], []) }

expr:
  | Lident paramlist                           { Eappl (pos $1, unpos $1, $2, EFall) }
  | Lident                                     { Eid (pos $1, unpos $1) }
  | Lident Lputvar patom stmttail              { Eputvar (pos $1, unpos $1, $3, $4, EFall) } 
  | Lident Lgetvar Lident stmttail             { Egetvar (pos $1, unpos $1, unpos $3, $4, EFall) } 
  | stmt                                       { $1 }

paramlist:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi eatom                                { [$2]   }
  | stmt                                       { [$1] }

paramlisttail:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi eatom                                { [$2]   }
  | stmt                                       { [$1] }
  |                                            { [] }

stmt:
  | Llambda idlist stmttail                    { Elambda ($1,$2,$3,EFall) }
  
stmttail:
  | Lsemi eatom                                { $2 }
  | atom                                       { $1 }
  
idlist:
  | Lident idlist                              { last_pos := pos $1;(unpos $1)::$2 }
  |                                            { [] }

eatom:
  | expr                                       { $1 }
  | atom                                       { $1 }

patom:
  | Lident                                     { last_pos := pos $1; Eid (pos $1,unpos $1) }
  | atom                                       { $1 }

atom:
  | Lint                                       { last_pos := pos $1; Eint (pos $1,unpos $1) }
  | Lstring                                    { last_pos := pos $1; Estring (pos $1,unpos $1) }
  | Llpar expr Lrpar                           { $2 }
 -}
  
  
