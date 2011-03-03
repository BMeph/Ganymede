{------------------------------------------------------------
(************************************************************ 
 Io Default Primitive Continuation Functions                  
   Based on:
   Amalthea, ml             
   Martin Sandin (d97masa@dtek.chalmers.se)
   translated to Haskell by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)  
 ************************************************************)
-------------------------------------------------------------}
module Io.Primitives where

import Io.Util ((.>), int_of_string, print_, subStr)
import Io.Types

import Control.Exception (throw)
import Data.List (genericLength)
import Data.HashMap.Lazy as M (HashMap, fromList) -- for commandMap
import System.IO.Unsafe (unsafePerformIO)

-- PrimFuncs :: [IoCont] -> (IoCont,[IoCont])
-- I.e., the internal values of IoCont's CPrim.
-- The argument is "the operand stack", the portion of
--  the execution stack looked at for the purpose of
--  running the function. The result is a pair of the
-- main result, and the unused part of the stack.

terminate     :: PrimFunc
addInt        :: PrimFunc
multInt       :: PrimFunc
powInt        :: PrimFunc
divInt        :: PrimFunc
subInt        :: PrimFunc
eqCont        :: PrimFunc
ltInt         :: PrimFunc
gtInt         :: PrimFunc
printInt      :: PrimFunc
printInt_     :: PrimFunc
stringFromInt :: PrimFunc
intFromString :: PrimFunc
concString    :: PrimFunc
subString     :: PrimFunc
stringLength  :: PrimFunc
printString   :: PrimFunc
printString_  :: PrimFunc
writeStar     :: PrimFunc
write         :: PrimFunc
writeLn       :: PrimFunc

terminate _  = (CTerminate, [])

{-
-- (* Concurrency operators: par, chan *)
parExec [cont_alt, c] = undefined
parExec       _       = throw (IoPrimError "par")

mkChan [c] = let x = supplyChanID in (c, [CChanIn x, CChanOut x])
mkChan _  = throw (IoPrimError "create chan")
-}

-- (* Integer operations *)
addInt [CInt na, CInt nb, c] = (c, [CInt (na + nb)])
addInt       _               = throw (IoPrimError "+")

multInt [CInt na, CInt nb, c] = (c, [CInt (na * nb)])
multInt             _         = throw (IoPrimError "*") 

powInt [CInt na, CInt nb, c] = (c, [CInt (na ^ nb)])
powInt            _          = throw (IoPrimError "^") 

divInt [CInt na, CInt nb, c]
   |  nb == 0  = throw (IoPrimError "/: division by zero")
   | otherwise = (c, [CInt (div na nb)])
divInt _ = throw (IoPrimError "/") 
  
subInt [CInt na, CInt nb, c] = (c, [CInt (na - nb)])
subInt _ = throw (IoPrimError "-") 
  
eqCont [CInt na, CInt nb, ctrue, cfalse] = 
    (if na == nb then ctrue else cfalse, [])
eqCont [CString na, CString nb, ctrue, cfalse] =
    (if na == nb then ctrue else cfalse, [])
eqCont [_, _, _, cfalse] = (cfalse,[])
eqCont _                     = throw (IoPrimError "=")

ltInt [CInt na, CInt nb, ctrue, cfalse] = (if na < nb then ctrue else cfalse, [])
ltInt _ = throw (IoPrimError "<")

gtInt [CInt na, CInt nb, ctrue, cfalse] = (if na > nb then ctrue else cfalse, [])
gtInt _ = throw (IoPrimError ">")

printInt [CInt n, c] = unsafePerformIO (print n .> (c, []))
printInt _ = throw (IoPrimError "print_int")

printInt_ [CInt n, c] = unsafePerformIO (print_ n .> (c, []))
printInt_      _      = throw (IoPrimError "print_int_")

stringFromInt [CInt n, c] = (c, [CString (show n)])
stringFromInt      _      = throw (IoPrimError "string_of_int")

intFromString [CString n, c] = case int_of_string n of
    Just x ->  (c, [CInt x])
    _     -> throw (IoPrimError "int_of_string: string not valid integer")
intFromString _ = throw (IoPrimError "int_of_string")

-- (* String operation *)
concString [CString sa, CString sb, c] =
    (c, [CString (sa ++ sb)])
concString _ = throw (IoPrimError "++")

subString [CString sa, CInt is, CInt il, c] = 
  (c, [CString (subStr is il sa)])
subString    _    = throw (IoPrimError "substring")
  
stringLength [CString sa, c] = (c, [CInt (genericLength sa)])
stringLength         _       = throw (IoPrimError "string_length")
  
printString [CString s, c] = unsafePerformIO
    (print s .> (c, []))
printString       _        = throw (IoPrimError "print_string")

printString_ [CString s, c] = unsafePerformIO
    (print_ s  .> (c, []))
printString_         _      = throw (IoPrimError "print_string_")

writeStar [c] = write [c,CTerminate]
writeStar  _  = throw (IoPrimError "write*")

write [CString s, c] = printString_ [CString s, c]
write [CInt    n, c] = printInt_    [CInt    n, c]
write       _        = throw (IoPrimError "write")

writeLn [CString s, c] = printString [CString s, c]
writeLn [CInt    n, c] = printInt    [CInt    n, c]
writeLn        _       = throw (IoPrimError "writeLn")
  
commandMap :: M.HashMap String PrimFunc
commandMap = M.fromList  
  [("terminate", terminate),
   ("int_add",addInt),
   ("int_sub",subInt),
   ("int_mul",multInt),
   ("int_div",divInt),
   ("int_exp",powInt),
-- ("par",parExec),
-- ("chan",mkChan),
   ("eq",eqCont),
   ("int_lt",ltInt),
   ("int_gt",gtInt),
   ("int_print_endline",printInt),
   ("int_print",printInt_),
   ("string_concat",concString),
   ("write_endline",writeLn),
   ("write",write),
   ("write_and_stop",writeStar),
   ("string_print",printString_),
   ("int_to_string",stringFromInt),
   ("string_sub",subString),
   ("string_length",stringLength),
   ("string_to_int",intFromString)]

{-
open Io_types;;

(* Misc operations *)
let terminate = function
  | [] -> (Cterminate, [])
  | _ -> raise (Io_prim_error "terminate")

(* Integer operations *)
let addint = function
  | [Cint na; Cint nb; c] -> (c, [Cint (na + nb)])
  | _ -> raise (Io_prim_error "+")

let multint = function
  | [Cint na; Cint nb; c] -> (c, [Cint (na * nb)])
  | _ -> raise (Io_prim_error "*") 

let divint = function
  | [Cint na; Cint nb; c] ->
    (try
      (c, [Cint (na / nb)])
    with
      | Division_by_zero -> raise (Io_prim_error "/: division by zero"))
  | _ -> raise (Io_prim_error "/") 
  
let subint = function
  | [Cint na; Cint nb; c] -> (c, [Cint (na - nb)])
  | _ -> raise (Io_prim_error "-") 
  
let eqcont = function
  | [Cint na;Cint nb; ctrue; cfalse] -> ((if na = nb then ctrue else cfalse), [])
  | [Cstring na; Cstring nb; ctrue; cfalse] -> ((if na = nb then ctrue else cfalse), [])
  | [ca; cb; ctrue; cfalse] -> (cfalse,[])
  | _ -> raise (Io_prim_error "=")

let ltint = function
  | [Cint na; Cint nb; ctrue; cfalse] -> ((if na < nb then ctrue else cfalse), [])
  | _ -> raise (Io_prim_error "<")

let gtint = function
  | [Cint na; Cint nb; ctrue; cfalse] -> ((if na > nb then ctrue else cfalse), [])
  | _ -> raise (Io_prim_error ">")

let printint = function
  | [Cint n; c] -> print_int n;print_newline (); (c, [])
  | _ -> raise (Io_prim_error "print_int")

let printint_ = function
  | [Cint n; c] -> print_int n; (c, [])
  | _ -> raise (Io_prim_error "print_int_")

let stringfromint = function
  | [Cint n; c] -> (c, [Cstring (string_of_int n)])
  | _ -> raise (Io_prim_error "string_of_int")

let intfromstring = function
  | [Cstring n; c] ->
    (try
      (c, [Cint (int_of_string n)])
    with
      Failure _ -> raise (Io_prim_error "int_of_string: string not valid integer"))
  | _ -> raise (Io_prim_error "int_of_string")

(* String operation *)
let concstring = function
  | [Cstring sa; Cstring sb; c] -> (c, [Cstring (sa ^ sb)])
  | _ -> raise (Io_prim_error "^")

let substring = function
  | [Cstring sa; Cint is; Cint il; c] -> (c, [Cstring (String.sub sa is il)])
  | _ -> raise (Io_prim_error "substring")
  
let string_length = function
  | [Cstring sa; c] -> (c, [Cint (String.length sa)])
  | _ -> raise (Io_prim_error "string_length")
  
let printstring = function
  | [Cstring s; c] -> print_endline s; (c, [])
  | _ -> raise (Io_prim_error "print_string")

let printstring_ = function
  | [Cstring s; c] -> print_string s; (c, [])
  | _ -> raise (Io_prim_error "print_string_")
  
let map =
  [("terminate", terminate);
   ("int_add",addint);
   ("int_sub",subint);
   ("int_mul",multint);
   ("int_div",divint);
   ("eq",eqcont);
   ("int_lt",ltint);
   ("int_gt",gtint);
   ("int_print_endline",printint);
   ("int_print",printint_);
   ("string_concat",concstring);
   ("string_print_endline",printstring);
   ("string_print",printstring_);
   ("int_to_string",stringfromint);
   ("string_sub",substring);
   ("string_length",string_length);
   ("string_to_int",intfromstring);]

 -} 

{-
Observations:
OCaml lets you mix effectful statements with expressions.
 This will "force" this section to return an (IO a) type.
Perhaps not OCaml, but the author did not care for using
 the CPS of the interpreted language inside the inter-
 preter itself. I will reserve judgement on if this was
 a good design choice. I definitely makes it clear when
 details of the internals, versus the interpreted items,
 are being used.
This seems to be a very OO method (no pun intended) to do
 interpretation. For instance, all of these "primitive"
 functions could be in a record, instead of a map.

 -}
 
 
