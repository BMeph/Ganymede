{-
(************************************************************ 
 Io Interpreter                                               
   Based on:
   Amalthea, io_interpreter.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   Translated to Haskell in 2011 by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
-------------------------------------------------------------} 
module Io.Interpreter where

import Io.Util (freshPos, (<--), (=:), fetch, ref)
import Io.Types
import Io.Env (bind, build_cont, massage_expr, safe_lookup, vLookup)
import Io.Messages as Messages ( print_lambda_trace
                   , application_trace
                   , enfilter_trace
                   , primitive_trace
                   -- error messages
                   , id_undefined_error
                   , parameter_mismatch_error
                   , primitive_error
                   )
   

import Prelude hiding (catch, exp)
import Control.Exception (Handler(..), catches, throw)
import Data.List (foldl')
import Data.Map as M (filterWithKey)
import Debug.Trace (trace)

{- Debug status vars -}
appl_pos :: IORef Pos
appl_pos = ref freshPos

{- (* Environment filter, removes everything but free variables in the rest of the
     expression from the environment. *) -}
filter_en  :: EnvFilter -> Bool -> IoEnv -> IoEnv
filter_en EFail _ ten = ten
filter_en (EFLimit filtIs) trace_flag (en, ven) = 
      (if trace_flag
        then trace (Messages.enfilter_trace filtIs)
        else id)
      (M.filterWithKey (\i _ -> elem i filtIs) en, ven)
        
-- (* Interpreter *)
interpret :: (IoCont, [IoCont]) -> Bool -> IO ()
interpret (CTerminate,_) _ = return ()
interpret (CPrim f, ps) trace_flag =
  (if trace_flag
    then trace (Messages.primitive_trace (fetch appl_pos))
    else id)
  interpret (f ps) trace_flag
interpret (CExpr (EAppl p i es flt) en _, []) trace_flag =
  let new_en = filter_en flt trace_flag en in do {
    appl_pos <-- p;
    (if trace_flag
      then trace (Messages.application_trace p i)
      else id)
      interpret (safe_lookup en i, map (build_cont False new_en) es) trace_flag }
interpret (CExpr (ELambda p is e flt) en _, ps) trace_flag =
  let iLen = length is
      pLen = length ps in
  if (iLen /= pLen)
   then throw (IoMismatchError "lambda" p iLen pLen)
   else
     let new_en = filter_en flt trace_flag en
         cmb = zip is ps
         en' = foldl' bind new_en cmb in
     foldr trace id (if trace_flag then Messages.print_lambda_trace p cmb else [])
       interpret (build_cont False en' e, []) trace_flag
interpret (CExpr (EGetVar _ vi i e _) en _, _) trace_flag = 
  let nen = bind en (i, fetch (vLookup en vi)) in
  interpret (build_cont False nen e, []) trace_flag
interpret (CExpr (EPutVar _ vi a e _) en _, _) trace_flag = do {
  (vLookup en vi =: build_cont False en a) `seq` 
  interpret (build_cont False en e, []) trace_flag
  }
interpret (CExpr (EAppl p _ _ _) _ _, ps) _ =
  throw (IoMismatchError "appl" p 0 (length ps))
interpret _ _ = throw IoASTError


-- (* Interprets and catches Io error exceptions and prints them. *)
catch_interpreter_errors :: (b -> IO a) -> b -> IO a
catch_interpreter_errors f c = catches (f c)
  [ Handler (\(IoMismatchError form p exp got) ->
         throw (IoError (Messages.parameter_mismatch_error
          (fetch appl_pos) form p exp got "")))
  , Handler (\(IoUndefinedError i) ->
        throw (IoError (Messages.id_undefined_error (fetch appl_pos) i "")))
  , Handler (\(IoPrimError m) ->
        throw (IoError (Messages.primitive_error m)))]

interpret_expr_in_env :: (IoEnv, IoExpr, (Bool, Bool)) -> IO ()
interpret_expr_in_env (en, ex, (mutable, traced)) =
  interpret ((build_cont True en . snd . massage_expr mutable) ex, []) traced


{-
(************************************************************ 
 Io Interpreter                                               
   Amalthea, io_interpreter.ml                                
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)
 
open Io_types;;
open Io_primitives;;
open Io_env;;


(* Debug status vars *)
let trace_flag = ref false
let appl_pos = ref ("", 0, 0)
let lambda_pos = ref ("", 0, 0)


(* Environment filter, removes everything but free variables in the rest of the
   expression from the environment. *)
let filter_en
  : en_filter -> io_env -> io_env
= fun flt ((en, ven) as ten) -> match flt with
  | EFall -> ten
  | EFlimit fltis ->
      if !trace_flag then
        Io_messages.print_enfilter_trace fltis;
      (List.filter (function (i, _) -> List.mem i fltis) en, ven)

        
(* Interpreter *)
let rec interpret : Io_types.io_cont * Io_types.io_cont list -> unit
= function 
  | (Cprim f, ps) ->
      if !trace_flag then
        Io_messages.print_primitive_trace !appl_pos;
      interpret (f ps)
  | (Cexpr (Eappl (p, i, es, flt), en, _), []) ->
      appl_pos := p;
      if !trace_flag then
        Io_messages.print_application_trace !appl_pos i;
      let new_en = filter_en flt en in
      interpret (safe_lookup en i, List.map (build_cont false new_en) es)
  | (Cexpr (Elambda (p, is, e, flt), en, _), ps) ->
      lambda_pos := p;
      let cmb = try List.combine is ps
        with Invalid_argument a ->
          raise (Io_interpreter_error (Io_mismatch_error
              ("lambda", p, (List.length is, List.length ps)))) in
      let
        en =
          (if !trace_flag then
            Io_messages.print_lambda_trace p cmb);
            let new_en = filter_en flt en in
            List.fold_left bind new_en cmb
      in
      interpret (build_cont false en e, [])
  | (Cexpr (Egetvar (p, vi, i, e, flt), en, _), ps) ->
    let nen = bind en (i, !(vlookup en vi)) in
    interpret (build_cont false nen e, [])
  | (Cexpr (Eputvar (p, vi, a, e, flt), en, _), ps) ->
    vlookup en vi := build_cont false en a;
    interpret (build_cont false en e, [])
  | (Cterminate,ps) -> ()
  | (Cexpr (Eappl (p, _, _, _), _, _), ps) ->
      raise (Io_interpreter_error
        (Io_mismatch_error ("appl", p, (0, List.length ps))))
  | (_,_) ->
      raise Amalathea_AST_error


(* Interprets and catches Io error exceptions and prints them. *)
let catch_interpreter_errors
= fun f c ->
  try
    f c
  with
    | Io_interpreter_error (Io_mismatch_error (form, p, (exp ,got))) ->
        raise (Io_error (Io_messages.parameter_mismatch_error
          !appl_pos (form, p, (exp ,got))));
    | Io_interpreter_error (Io_undefined_error i) ->
        raise (Io_error (Io_messages.id_undefined_error !appl_pos i))
    | Io_prim_error m ->
        raise (Io_error (Io_messages.primitive_error m))

let interpret_expr_in_world : io_world * io_expr -> unit
= function ((_ , en, _), ex) ->
  interpret (Io_env.build_cont true en (snd (Io_env.massage_expr ex)), [])

 -}
