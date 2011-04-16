{------------------------------------------------------------
(************************************************************ 
 Io Source Pretty Printer                                     
   Based on:   
   Amalthea, io_pretty.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   translated to Haskell by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)   
 ************************************************************)
-------------------------------------------------------------}
module Io.Pretty where

import Io.Types
import Io.Util (interCC)

import Data.List (nub)

prettyPrint :: IoAST -> ShowS
prettyPrint (imports, exports, (ds, _), e) = ppImport (nub imports) .
  ppExport exports . ppDecl ds . ppExpr "" e
    
ppDecl :: [IoDecl] -> ShowS
ppDecl = foldr (\ (_, i, e) b -> (i++) . (": "++) .
  ppExpr "  " e . showString ".\n\n" . b) id
          
ppExport :: [IoID] -> ShowS
ppExport [] = id
ppExport is = ("export " ++) . interCC " " is . (".\n\n"++)

ppImport :: [IoImport] -> ShowS
ppImport = foldr ((.) . ppImp) ("\n"++) where
    ppImp (i, j) = (".\n"++) . maybeImpList j . impHeader i
    impHeader i  = ("import "++) . (i++)
    maybeImpList = maybe id (((':':) .). interCC " ")
-- ppImport  []                   = ("\n"++)
-- ppImport ((i, Nothing) : imps) = ppImp (i, Nothing) . ppImport imps
-- ppImport ((i, Just is) : imps) = ppImp (i, Just is) . ppImport imps
--   maybeImpList Nothing = id
--   maybeImpList (Just is) = (':':) . interCC " " is

  
{- Pretty print an expression -}     
ppExpr :: String -> IoExpr -> ShowS
ppExpr pfix ioExpr = case ioExpr of
    ELambda _ is e _ -> ("-> "++) . interCC " " is .
      ppLastAction pfix e
    EPutVar _ i e e2 _ -> (i++) . showString " <<= " .
      ppAtom pfix e . ppLastAction pfix e2
    EGetVar _ i i2 e2 _ -> (i++) . (" =>> "++) . (i2++) .
      ppLastAction pfix e2
    EAppl _ i es _ -> (i++) . ppAppLp pfix es
    EId _ i -> (i++)
    EInt _ n -> shows n
    EString _ s -> shows s
    EPrimitive _ -> ("<<primitive>>"++)

{- Pretty print an application parameter list -}   
ppAppLp :: String -> [IoExpr] -> ShowS
ppAppLp pfix ps = case ps of
    [ ]                   -> id
    [e@(ELambda _ _ _ _)] -> (' ':) . ppExpr pfix e
    [e]                   -> ppLastAction pfix e
    (e:es)                -> (' ':) . ppAtom pfix e . ppAppLp pfix es

{- Pretty print an atom -}   
ppAtom :: String -> IoExpr -> ShowS
ppAtom pfix eAtom = case eAtom of
    EId _ i     -> (i++) 
    EInt _ n    -> shows n
    EString _ s -> shows s
    e           -> showParen True (tail . ppLastAction (pfix ++ "  ") e)
 
{- Pretty prints a terminal action -}   
ppLastAction :: String -> IoExpr -> ShowS
ppLastAction pfix expr = (";\n" ++) .
      (pfix ++) . ppExpr pfix expr
{-
(************************************************************ 
 Io Source Pretty Printer                                     
   Amalthea, io_pretty.ml                                     
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)

open Io_types;;

(* Pretty print a program *)
let rec pretty_print
  : Io_types.io_ast -> string
= function
  | (imp, exp, (ds, _), e) ->
      pp_import imp
      ^ pp_export exp
      ^ pp_decl ds
      ^ pp_expr "" e ^ "\n"

(* Pretty print a declaration *)
and pp_decl
  : io_decl list -> string
= function
  | []        -> ""
  | (_,i,e) :: ds ->
      "declare " ^ i ^ ": " ^ pp_expr "  " e ^ ".\n\n" ^ pp_decl ds

and pp_export
  : io_id list -> string
= function
  | [] -> ""
  | is -> "export " ^ String.concat " " is ^ ".\n\n"

and pp_import
  : io_import list -> string
= function
  | [] -> "\n"
  | ((i, None) :: imps) -> "import " ^ i ^ ".\n" ^ pp_import imps
  | ((i, Some is) :: imps) ->
      "import: " ^ String.concat " " is ^ ".\n" ^ pp_import imps
  
(* Pretty print an expression *)     
and pp_expr
  : string -> io_expr -> string
= fun pfix e -> match e with
  | Elambda (_,is,e,_) -> "-> "
      ^ String.concat " " is
      ^ ";\n" ^ pfix ^ pp_expr pfix e
  | Eputvar (_,i,e,e2,_) ->
      i ^ " <<= " ^ pp_atom pfix e ^ ";\n"
      ^ pfix ^ pp_expr pfix e2
  | Egetvar (_,i,i2,e2,_) ->
      i ^ " =>> " ^ i2 ^ ";\n"
      ^ pfix ^ pp_expr pfix e2
  | Eappl (_,i,es,_) -> i ^ pp_applp pfix es
  | Eid (_,i) -> i
  | Eint (_,n) -> string_of_int n
  | Estring (_,s) -> "\"" ^ s ^ "\""
  | Eprimitive i -> raise Not_found

(* Pretty print an application parameter list *)   
and pp_applp
  : string -> io_expr list -> string
= fun pfix ps -> match ps with
  | [] -> ""
  | ((Elambda _) as e)::[] -> " " ^ pp_expr pfix e
  | e::[] -> ";\n" ^ pfix ^ pp_expr pfix e
  | e::es -> " " ^ pp_atom pfix e ^ pp_applp pfix es

(* Pretty print an atom *)   
and pp_atom
  : string -> io_expr -> string
= fun pfix ps -> match ps with
  | Eid (_,i) -> i 
  | Eint (_, n) -> string_of_int n
  | Estring (_, s) -> "\"" ^ s ^ "\""
  | e -> "(\n" ^ pfix ^ "  " ^ pp_expr (pfix ^ "  ") e ^ ")"
  
 -}
 
