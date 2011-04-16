{- #  # -}
-- module Io.Parser where
module Io.Parser where

import Io.Types
import Io.Lexer as Lex
import Io.Util ((=:), (/>), freshPos, ref)

import Control.Exception (throw)
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Text.Parsec 
import Text.Parsec.String (Parser)
import qualified Text.Parsec.String as S (parseFromFile)
import Text.Parsec.Token as T ()

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile = S.parseFromFile

lastPos :: IORef Pos
lastPos = ref freshPos
-- last_pos = IoParserState "" 0 0

parse_error :: Pos -> t -> a
parse_error last_pos _  = throw (IoParserError last_pos)

-- start_parser = ParseIoModule ([],[],([],[]), EPrimitive "")

{-
parse_io_module:
  | imports export decll expr Lend             { ($1,$2,$3,$4) }
 -}

parse_io_module :: Parser IoAST
parse_io_module = do
  { is <- parse_imports
  ; es <- parse_exports
  ; dPair <- decls
  ; ps <- expr
  ; end
  ; return (is,es,dPair,ps)
  }

{-
imports:
  | Limport Lident importlist Lperiod imports  { last_pos := pos $2;(String.lowercase (unpos $2), $3)::$5 }
  |                                            { [] }
-}

parse_imports :: Parser IoImports
parse_imports = many (do
 { _ <- Lex.modImp
 ; (p,mName) <- Lex.ident
 ; iList <- impList
 ; Lex.period
 ; return (lastPos =: p `seq` (map toLower mName, iList))
 }) <?> "import statement(s)"

{-
importlist:
  | Lcolon idlist                              { Some $2 }
  |                                            { None }
-}
impList :: Parser (Maybe [IoID])
impList = optionMaybe (Lex.fColon >> idList)


{-
export:
  | Lexport idlist Lperiod                     { $2 }
  |                                            { [] }
-}

parse_exports :: Parser IoExports
parse_exports = option [] (
  between Lex.modExport Lex.period idList) <?> "export statement"

{-
 decll:
  | Ldeclare Lident Lcolon eatom Lperiod decll    { last_pos := pos $2;ct1 ($1,unpos $2,$4) $6 }
  | Lprimitive Lident Lcolon Lident Lperiod decll { last_pos := pos $2;ct1 ($1,unpos $2,Eprimitive (unpos $4)) $6 }
  | Lvariable Lident Lcolon eatom Lperiod decll   { last_pos := pos $2;ct2 ($1,unpos $2,$4) $6 }
  |                                               { ([], []) }
 -}
  
decls :: Parser ([IoDecl], [IoDecl])
decls = fmap partitionEithers (many (declStmt <|> primStmt <|> varStmt) <?> "declaration") where
  declStmt = do
    { decPos <- Lex.declare
    ; (p,i) <- nameDef
    ; ea <- eAtom
    ; Lex.period
    ; return (lastPos =: p `seq` Left (decPos, i, ea))
    } <?> "normal declaration"
  primStmt = do
    { priPos <- Lex.primitive
    ; (p,i) <- nameDef
    ; (_,id2) <- Lex.ident
    ; Lex.period
    ; return (lastPos =: p `seq` Left (priPos, i, EPrimitive id2))
    } <?> "primitive declaration"
  varStmt  = do
    { varPos <- Lex.variable
    ; (p,i) <- nameDef
    ; ea <- eAtom
    ; Lex.period
    ; return (lastPos =: p `seq` Right (varPos, i, ea))
    } <?> "variable declaration"
  nameDef = try (Lex.ident /> Lex.fColon)

{-
expr:
  | Lident Lputvar patom stmttail              { Eputvar (pos $1, unpos $1, $3, $4, EFall) } 
  | Lident Lgetvar Lident stmttail             { Egetvar (pos $1, unpos $1, unpos $3, $4, EFall) } 
  | Lident paramlist                           { Eappl (pos $1, unpos $1, $2, EFall) }
  | Lident                                     { Eid (pos $1, unpos $1) }
  | stmt                                       { $1 }
 -}
 
expr :: Parser IoExpr
expr = (stmt <|> do
 { (p,i) <- Lex.ident
 ; (do
   { _ <- Lex.putVar
   ; pa <- pAtom
   ; stt <- stTail
   ; return (EPutVar p i pa stt EFail)
   } <?> "variable store expr")
 <|> (do
   { _ <- Lex.getVar
   ; (_,id2) <- Lex.ident
   ; stt <- stTail
   ; return (EGetVar p i id2 stt EFail)
   } <?> "variable load expr")
 <|> (do
   { ps <- pList
   ; return (EAppl p i ps EFail)
   } <?> "application expr")
 <|> (return (EId p i) <?> "bound argument")
 }) <?> "expression"

{-
idlist:
  | Lident idlist                              { last_pos := pos $1;(unpos $1)::$2 }
  |                                            { [] }
-}

-- Note: idList is defined in this way, to imply and encourage
--  the interpretation that the 'lastPos' reference holds the
--  starting pos of the list.
idList :: Parser [IoID]
idList = many (do
 { (p,i) <- Lex.ident
 ; return (lastPos =: p `seq` i)
 }) <?> "identified list"

{-
paramlist:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi expr                                 { [$2]   }
  | stmt                                       { [$1]   }
-}

pList :: Parser [IoExpr]
pList = ((do
 { st <- stmt
 ; return [st]
 })
 <|> (do
 { stT <- stTail
 ; return [stT]
 } <?> "final action")
 <|> (do
 { p <- pAtom <?> "par atom"
 ; ps <- pTail
 ; return (p:ps)
 })) <?> "param list"

{-
paramlisttail:
  | stmt                                       { [$1]   }
  | Lsemi expr                                 { [$2]   }
  | patom paramlisttail                        { $1::$2 }
  |                                            { [  ]   }
-}

pTail :: Parser [IoExpr]
pTail = (option [] pList) <?> "param list tail"
  
{-
stmt:
  | Llambda idlist stmttail                    { Elambda ($1,$2,$3,EFall) }
-}
  
stmt :: Parser IoExpr
stmt = do
  { p <- Lex.lambda
  ; is <- idList
  ; ioex <- stTail
  ; return (ELambda p is ioex EFail)
  } <?> "(lambda) statement"
  
{-  
stmttail:
  | Lsemi expr                                { $2 }
 -} 

stTail :: Parser IoExpr
stTail = (semiC >> expr) <?> "continuation"

{-
eatom:
  | expr                                       { $1 }
  | atom                                       { $1 }
-}

eAtom :: Parser IoExpr
eAtom = (expr <|> atom) <?> "expr/atom"

{-
patom:
  | Lident                                     { last_pos := pos $1; Eid (pos $1,unpos $1) }
  | atom                                       { $1 }
-}

  
pAtom :: Parser IoExpr
pAtom = ((do
 { (p,i) <- Lex.ident
 ; return (lastPos =: p `seq` EId p i)
 } <?> "atom parameter")
 <|> atom) <?> "param atom"

{-
atom:
  | Llpar expr Lrpar                           { $2 }
  | Lstring                                    { last_pos := pos $1; Estring (pos $1,unpos $1) }
  | Lint                                       { last_pos := pos $1; Eint (pos $1,unpos $1) }
-}

atom :: Parser IoExpr
atom = ((Lex.parens expr <?> "parenthized action")
 <|> (do
  { (p,s) <- Lex.string
  ; return (lastPos =: p `seq` EString p s)
  } <?> "string atom")
  <|> (do
  { (p,i) <- Lex.int
  ; return (lastPos =: p `seq` EInt p i)
  } <?> "int atom")) <?> "atom"

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
  
  
