{------------------------------------------------------------
(************************************************************ 
 Io Error and Trace Messages
   Based on:
   Amalthea, io_messages.ml                                   
   Martin Sandin (d97masa@dtek.chalmers.se)
   translated in 2011 to Haskell by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
-------------------------------------------------------------}

module Io.Messages where

import Io.Types
import Io.Util (fetch, showBracket, showsPos)

import Control.Exception (throw)
import Data.List (foldl', intercalate)
import System.FilePath (takeBaseName)

{- Returns the size of a continuation as a tuple of the number of
   continuations bound in recursive environments and the number of
   conts bound in this continuation's environment. Declared and
   primitive conts excluded. -}
-- cont_size's IoCont "should" be an Io CExpr
cont_size :: IoCont -> (Int, Int)
cont_size (CExpr _ (en, _) False) =
    (foldl' ((.(fst . cont_size . fetch)) . (+)) 1 $ map snd en, length en) 
    -- \ total (_,c) -> total + (fst (cont_size (fetch c)))
cont_size (CExpr _ _ True) = (0,0)
cont_size         _        = (1,0)


{- (* Trace messages 
 ************************************************************)
 -}
 
-- (* Prints continuations to the desired depth *)
print_cont :: Int -> IoCont -> String
print_cont level c = case c of
    CExpr ex _ _ -> print_expr level ex
    CInt n       -> show n
    CString s    -> show s
    CPrim _      -> "<primitive>"
    _            -> throw (IoError "Trace error: erroneous continuation.")
  
print_expr :: Int -> IoExpr -> String
print_expr 0 _ = "..."
print_expr level e' = case e' of {
      ELambda _ is e _ -> "-> " ++ intercalate " " is ++
          ";" ++ print_expr (level - 1) e;
      EAppl _ i es _ -> i ++ printApplParams (level - 1) es;
      EPutVar _ i e e2 _ -> i ++ " <<=" ++ printParam (level - 1) e ++ ";" ++ print_expr (level - 1) e2;
      EGetVar _ i i2 e2 _ -> i ++ " =>> " ++ i2 ++ ";" ++ print_expr (level - 1) e2;
      EId _ i -> i;
      EInt _ n -> show n;
      EString _ s -> show s;
      EPrimitive _ -> throw IoASTError } where
    printApplParams :: Int -> [IoExpr] -> String
    printApplParams lvl ps = case ps of
          [] -> ""
          [e@(ELambda _ _ _ _)] -> " " ++ print_expr lvl e
          [e] -> ";" ++ print_expr lvl e
          (e:es) -> printParam lvl e ++ printApplParams lvl es
    printParam :: Int -> IoExpr -> String
    printParam lvl ps = " " ++ case ps of
          EId _ i      ->  i
          EInt _ n     -> show n
          EString _  s -> show s
          e            -> "(" ++ print_expr lvl e ++ ")"

-- (* Prints lambda expression trace messages *)
print_lambda_trace :: Pos -> [(IoID, IoCont)] -> [String]
print_lambda_trace p cmb = ("  " ++ showsPos p ": lambda called") :
  (map (\ (i,c) -> let (s,es) = cont_size c in
      "    " ++ i ++ " := " ++ print_cont 3 c ++ 
         (' ': showBracket (shows es . ('/':) . shows s) "")) cmb)

enfilter_trace :: [String] -> String
enfilter_trace fltis = ("    "++) $
  showParen True (intercalate ", " fltis ++) ""

application_trace :: Pos -> IoID -> String
application_trace pos i = ("  " ++) . showsPos pos .
 (": application of '" ++) . (i ++) $ "'"

primitive_trace :: Pos -> String
primitive_trace pos = "  " ++ showsPos pos ": primitive call"


{- (* Module error messages 
 ************************************************************)
 -}
parser_error :: Pos -> String
parser_error p = "Parse error after " ++ showsPos p ""

lexer_error :: Pos -> String
lexer_error p = "Parse error: invalid character after " ++ showsPos p ""

module_not_found_error :: IoModuleName -> IoID -> String
module_not_found_error importer imported = ("Module error: "++) .
  ("could not locate "++) . (imported++) .
  (" imported by "++) $ importer
  
undefined_export_error :: IoID -> FilePath -> String
undefined_export_error exported mname = ("Module error: "++) .
  ("exported identifier undefined in "++) .
  (takeBaseName mname ++) . (": "++) $
  exported
  
undefined_import_error :: IoID -> FilePath -> String
undefined_import_error imported mname =
  ("Module error: "++) .
  ("imported identifier undefined in "++) .
  (takeBaseName mname ++) . (": "++) $
  imported
  
  
-- (* Interpreter error messages 
-- ************************************************************)
parameter_mismatch_error :: Pos -> String -> Pos -> Int -> Int -> ShowS
parameter_mismatch_error apl_pos form lambda_p expected got =
  ("Io error: call at "++) . showsPos apl_pos .
  (" to "++) . showString form . showString " at " . showsPos lambda_p .
  showString ": " . showString "expected " . shows expected .
  showString " parameter(s), got " . shows got
  
id_undefined_error :: Pos -> String -> ShowS
id_undefined_error pos i = ("Io error: "++) . showString i .
   (" undefined in appl at "++) . showsPos pos
  
primitive_error :: ShowS
primitive_error = ("Io primitive error: " ++)

{- The original OCaML:

(************************************************************ 
 Io Error and Trace Messages                                  
   Amalthea, io_messages.ml                                   
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)

open Io_types;;

(* Util *)
let string_of_pos : pos -> string
= function (m,r,c) ->
  Filename.basename m ^ " r" ^ string_of_int r ^ " c" ^ string_of_int  c

(* Returns the size of a continuation as a tuple of the number of
   continuations bound in recursive environments and the number of
   conts bound in this continuations environment. Declared and
   primitive conts excluded. *)
let rec cont_size = function
  | Cexpr (_,(en, ven),false) ->
    (1 + (List.fold_left (fun sum (_,c) -> sum + (fst (cont_size !c))) 0 en), List.length en)
  | Cexpr (_,_,true) -> (0,0)
  | _ -> (1,0)


(* Trace messages 
 ************************************************************)

(* Prints continuations to the desired depth *)
let rec print_cont
= fun level c -> match c with
  | Cexpr (ex, _, _) -> print_expr level ex
  | Cint n -> string_of_int n
  | Cstring s -> "\"" ^ s ^ "\""
  | Cprim _ -> "<primitive>"
  | _ -> raise (Io_error "Trace error: erronous continuation.")
  
and print_expr : int -> io_expr -> string
= fun level e -> match level with
  | 0 -> "..."
  | level -> match e with
    | Elambda (_,is,e,_) -> "-> "
        ^ String.concat " " is
        ^ ";" ^ print_expr (level - 1) e
    | Eappl (_,i,es,_) -> i ^ print_applparams (level - 1) es
    | Eputvar (_,i,e,e2,_) -> i ^ " <<=" ^ print_param (level - 1) e ^ ";" ^ print_expr (level - 1) e2
    | Egetvar (_,i,i2,e2,_) -> i ^ " =>> " ^ i2 ^ ";" ^ print_expr (level - 1) e2
    | Eid (_,i) -> i
    | Eint (_, n) -> string_of_int n
    | Estring (_,s) -> "\"" ^ s ^ "\""
    | Eprimitive i -> raise Amalathea_AST_error

and print_applparams: int -> io_expr list -> string
= fun level ps -> match ps with
    | [] -> ""
    | ((Elambda _) as e)::[] -> " " ^ print_expr level e
    | e::[] -> ";" ^ print_expr level e
    | e::es -> print_param level e ^ print_applparams level es

and print_param: int -> io_expr -> string
= fun level ps -> match ps with
    | (Eid (_,i)) -> " " ^ i
    | (Eint (_,n)) -> " " ^ string_of_int n
    | (Estring (_,s)) -> " \"" ^ s ^ "\""
    | e -> " (" ^ print_expr level e ^ ")"

(* Prints lambda expression trace messages *)
let print_lambda_trace
= fun p cmb ->
  print_endline ("  " ^ string_of_pos p ^ ": lambda called");
  List.iter (function (i,c) ->
    let (s,es) = cont_size c in
    print_endline ("    " ^ i
      ^ " := " ^ print_cont 3 c
      ^ " [" ^ string_of_int (es) ^ "/" ^ string_of_int (s) ^ "]")
  ) cmb

let print_enfilter_trace
= fun fltis -> print_endline ("    {" ^ String.concat ", " fltis ^ "}")

let print_application_trace 
= fun pos i -> print_endline ("  " ^ string_of_pos pos ^ ": application of '" ^ i ^ "'")

let print_primitive_trace 
= fun pos -> print_endline ("  " ^ string_of_pos pos ^ ": primitive call")


(* Module error messages 
 ************************************************************)
let parser_error
= fun p -> "Parse error after " ^ string_of_pos p

let lexer_error
= fun p -> "Parse error: invalid character after " ^ string_of_pos p

let module_not_found_error
= fun importer imported ->
  "Module error: "
  ^ "could not locate " ^ imported
  ^ " imported by " ^ importer
  
let undefined_export_error
= fun exported mname ->
  "Module error: "
  ^ "exported identifier undefined in "^Filename.basename mname
  ^": "^exported
  
let undefined_import_error
  = fun imported mname ->
    "Module error: "
    ^ "imported identifier undefined in "^Filename.basename mname
  ^": "^imported
  
  
(* Interpreter error messages 
 ************************************************************)
let parameter_mismatch_error
= fun apl_pos (form, lambda_p, (exp, got)) ->
  "Io error: call at " ^ string_of_pos apl_pos
  ^ " to " ^ form ^ " at " ^ string_of_pos lambda_p ^ ": "
  ^ "expected " ^ string_of_int exp
  ^ " parameter(s), got " ^ string_of_int got
  
let id_undefined_error
= fun pos i ->
  "Io error: " ^ i ^ " undefined in appl at "
  ^ string_of_pos pos
  
let primitive_error
= fun m ->
  "Io primitive error: " ^ m
 -}
