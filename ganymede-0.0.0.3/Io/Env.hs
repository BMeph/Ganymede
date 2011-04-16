{-# LANGUAGE PatternGuards, ViewPatterns #-}
{-
(************************************************************ 
 Io Environment Handling                                      
   Based on:
   Amalthea, io_messages.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   Translated in 2011 to Haskell by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
 -}
module Io.Env where

import qualified Io.Primitives as Prim (commandMap)
import Io.Types
import Io.Util ((<--), fetch, first, ref, second, uncIns)

import Prelude hiding (pi)
import Control.Exception (throw)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Map as M (Map, empty, filter, lookup, map, toList)

(!>) :: M.Map IoID a -> IoID -> a
m !> v = fromJust (M.lookup v m)

-- (* The standard environment *)
standard_environment :: IoEnv
standard_environment = (M.empty, M.empty)

-- (* Environment handling functions *)
envLookup :: IoEnv -> IoID -> IoCont
envLookup (en, _) i = fetch (en !> i)

vLookup :: IoEnv -> IoID -> IORef IoCont
vLookup (_, vEn) vI = fetch (vEn !> vI)
  
safe_lookup :: IoEnv -> IoID -> IoCont
safe_lookup (en, _) i = maybe (throw (IoUndefinedError i)) fetch $
                       M.lookup i en

bind   :: IoEnv -> IoAssoc  -> IoEnv
vBind  :: IoEnv -> IoVAssoc -> IoEnv
bind  = flip fBind 
vBind = flip fVBind

fBind  :: IoAssoc  -> IoEnv -> IoEnv
fVBind :: IoVAssoc -> IoEnv -> IoEnv
fBind  = first  . uncIns . second ref
fVBind = second . uncIns . second ref
-- ^ fBind  (i, c)  (en, vEn) = (uncIns (i, ref c) en, vEn                  )
-- ^ fVBind (vi, c) (en, vEn) = (en                  , uncInc (i, ref c) vEn)
 
updateRef :: IoEnv -> (IoID, IoCont) -> IO ()
-- (<--) :: IORef a -> a -> IO ()
updateRef  (en, _ ) (i, c) =  (en !> i)  <-- c

updateVRef :: IoEnv -> (IoID, IORef IoCont) -> IO ()
updateVRef (_, vEn) (vI,c) = (vEn !> vI) <-- c

{- Builds a continuation from an expression and a environment.
   decl: True for declared continuations, False for runtime constructed.
         This is used by the trace printer. -}
build_cont :: Bool -> IoEnv -> IoExpr -> IoCont
build_cont decl en ex = case ex of
    EInt _ n      -> CInt n
    EString _ s   -> CString s
    EId _ i       -> safe_lookup en i
    EPrimitive pi -> CPrim (Prim.commandMap !> pi)
    _             -> CExpr ex en decl


{- Finds free variables and returns the expression with an env filter. -}
-- massage_expr: MutableEnabled -> Expression -> (FreeVs,Eval'dExpression)
massage_expr :: Bool -> IoExpr -> ([IoID], IoExpr)
massage_expr mutOn (ELambda p is ex _) = (fis',ELambda p is e (EFLimit fis')) where
    (fis,e) = massage_expr mutOn ex
    fis' = Prelude.filter (flip notElem is) fis
massage_expr mutOn (EAppl p i es _) = (i:fis,EAppl p i es' (EFLimit fis)) where
    (iss, es') = unzip (map (massage_expr mutOn) es)
    fis = nub (concat iss)
massage_expr mutOn (EGetVar p vi i ex _)
  | not mutOn = massage_expr False ex
  | otherwise = let (fis, e) = massage_expr True ex
                    fis'     = Prelude.filter (/= i) fis
                in  (fis', EGetVar p vi i e (EFLimit fis'))
massage_expr mutOn (EPutVar p vi a ex _)
  | not mutOn = massage_expr False ex
  | otherwise = let (fis, e)   = massage_expr True ex
                    (afis, ae) = massage_expr True a
                    fis'       = nub (fis ++ afis)
                in  (fis', EPutVar p vi ae e (EFLimit fis'))
massage_expr _ ex@(EId _ i) = ([i], ex)
massage_expr _ ex = ([], ex)
{-
massage_expr mutOn = memO where
  memO (ELambda p is ex _) = (fis',ELambda p is e (EFLimit fis')) where
    (fis,e) = memO ex
    fis' = Prelude.filter (flip notElem is) fis
  memO (EAppl p i es _) = (i:fis,EAppl p i es' (EFLimit fis)) where
    (iss, es') = unzip (Prelude.map memO es)
    fis = nub (concat iss)
  memO (EGetVar p vi i ex _)
  | not mutOn = memO ex
  | otherwise = let (fis, e) = memO ex
                    fis'     = Prelude.filter (/= i) fis
                in  (fis', EGetVar p vi i e (EFLimit fis'))
  memO (EPutVar p vi a ex _)
  | not mutOn = memO ex
  | otherwise = let (fis, e)   = memO ex
                    (afis, ae) = memO a
                    fis'       = nub (fis ++ afis)
                in  (fis', EPutVar p vi ae e (EFLimit fis'))
  memO ex@(EId _ i) = ([i], ex)
  memO ex = ([], ex)

  
-}

{- Build local continuation and references it in the environment -}
build_local_conts :: Bool -> IoEnv -> IO ()
build_local_conts traceP ten@(en, ven) = mapM_ build (M.toList en) >>
                                           mapM_ vBuild (M.toList ven) where
  build (i, fetch -> CLocal ex)
    = updateRef ten (i, build_cont True ten (snd (massage_expr traceP ex)))
  build _ = return ()
  vBuild (i, fetch . fetch -> CLocal ex)
    = updateVRef ten (i, ref (build_cont True ten (snd (massage_expr traceP ex))))
  vBuild _ = return ()

{- Build local continuation and references it in the environment,
  the "update" version
localContUpdate :: Bool -> IoEnv -> IoEnv
localContUpdate traceP ten@(en, ven) =
 ((M.toList . M.mapWithKey (uncIns . build . getLExpr) . M.filter locals . M.map fetch) en,
              (M.mapWithKey (uncIns . vBuild . getLExpr) . M.filter locals . M.map (fetch . fetch)) ven) where
  locals (CLocal _) = True
  locals        _   = False
  getLExpr _ (CLocal e) = e
  build (i, ex)
    = updateRef ten (i, build_cont True ten (snd (massage_expr traceP ex)))
  vBuild (i, ex)
    = updateVRef ten (i, ref (build_cont True ten (snd (massage_expr traceP ex))))
 -}
 
{- References an external continuation i in en, hunts it down in the list
   of environments, ens. -}
link_external_conts :: [(IoID, IoEnv)] -> IoEnv -> IO ()
link_external_conts ens ten@(en, _) = mapM_ link . M.toList . M.filter extRef $
                                        M.map fetch en where
  extRef (CExternal _) = True
  extRef       _       = False
  link (i, _) = updateRef ten (i, get_cont [] (envLookup ten i)) where
    get_cont visited_ms (CExternal mname) =
      if mname `elem` visited_ms
        then throw (IoUndefinedExportError i mname)
        else get_cont (mname : visited_ms)
              (envLookup (fromJust (Prelude.lookup mname ens)) i)
    get_cont _ c = c

{- Returns an updated environment pair, given an ID/env list of possible substitutes.
extlContUpdates :: [(IoID, IoEnv)] -> IoEnv -> IoAssocs
extlContUpdates ens ten@(en, _) = foldr uncIns en . M.toList . M.map (get_cont [] . getEName) .
                                    M.filter extRef $ M.map fetch en where
  extRef (CExternal _) = True
  extRef         _     = False
  getEName (CExternal e) = e
  link (i, ce) = updateRef ten (i, get_cont [] ce)
  get_cont visited_ms (CExternal mname) =
    if mname `notElem` visited_ms
      then get_cont (mname : visited_ms)
            (envLookup (fromJust (Prelude.lookup mname ens)) i)
      else fetch ce
  get_cont _ c = c
   -}

{-
(************************************************************ 
 Io Environment Handling                                      
   Amalthea, io_messages.ml                                   
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)
open Io_types;;
open Io_parser;;


let variable_flag = ref false

let exported_ids = function 
  | (_, [], ds, _) -> List.map (fun (_, i, _) -> i) ds
  | (_, es, _, _)  -> es

(* The standard environment *)
let standard_environment : io_env
= ([], [])

(* Environment handling functions *)
let lookup = fun (en, ven) i ->
  !(List.assoc i en)

let vlookup = fun (_, ven) vi ->
  !(List.assoc vi ven)
  
let safe_lookup = fun en i ->
  try 
    lookup en i
  with Not_found ->
    raise (Io_interpreter_error (Io_undefined_error i))

let bind = fun (en, ven) (i, c) -> ((i, ref c)::List.remove_assoc i en, ven)
let vbind = fun (en, ven) (vi, c) -> (en, (vi, ref c)::List.remove_assoc vi ven)

let update_ref = fun (en, _) (i,c) ->
  (List.assoc i en) := c

let update_vref = fun (_, ven) (i,c) ->
  (List.assoc i ven) := c

(* Builds a continuation from an expression and a environment.
   decl: true for declared continuations, false for runtime constructed.
         This is used by the trace printer. *)
let build_cont
  : bool -> io_env -> io_expr -> io_cont
= fun decl en ex -> match ex with
  | Eint (p,n) -> Cint n
  | Estring (p,s) -> Cstring s
  | Eid (p,i) -> safe_lookup en i
  | Eprimitive pi -> Cprim (List.assoc pi Io_primitives.map)
  | _  -> Cexpr (ex, en, decl)


(* Finds free variables and returns the expressin with an env filter. *)
let rec massage_expr = function
  | Elambda (p, is, ex, _) ->
    let (fis,e) = massage_expr ex in
    let fis = List.filter (fun i -> not (List.mem i is)) fis in
    (fis,Elambda (p, is, e, EFlimit fis))
  | Eappl (p, i, es, _) ->
    let (iss, es) = List.split (List.map massage_expr es) in
    let iss = iss in
    let fis =
      List.fold_left (fun ais is ->
        ais @ (List.filter (fun i ->
          not (List.mem i ais)) is)) [] iss in
    (i::fis,Eappl (p, i, es, EFlimit fis))
  | Egetvar (p, vi, i, ex, _) ->
    if !variable_flag then
      let (fis, e) = massage_expr ex in
      let fis = List.filter (fun li -> li != i) fis in
      (fis, Egetvar (p, vi, i, e, EFlimit fis))
    else
      massage_expr ex
  | Eputvar (p, vi, a, ex, _) ->
    if !variable_flag then
      let (fis, e) = massage_expr ex in
      let (afis, ae) = massage_expr a in
      let fis = List.fold_left (fun is i -> if List.mem i is then is else i::is) fis afis in
      (fis, Eputvar (p, vi, ae, e, EFlimit fis))
    else
      massage_expr ex
  | (Eid (p, i)) as ex -> ([i], ex)
  | ex -> ([], ex)


(* Build local continuation and references it in the environment *)
let build_local_conts : io_env -> unit
= fun ((en, ven) as ten) ->
  let build (i, c) =
    match !c with
      | Clocal ex -> update_ref ten (i, build_cont true ten (snd (massage_expr ex)))
      | _ -> ()
  in let vbuild (i, c) =
    match !(!c) with
      | Clocal ex -> update_vref ten (i, ref (build_cont true ten (snd (massage_expr ex))))
      | _ -> ()
  in
    List.iter build en;
    List.iter vbuild ven


(* References an external continuation i in en, hunts it down in the list
   of environments, ens. *)
let link_external_conts : (string * io_env) list -> io_env -> unit
= fun ens ((en, _) as ten)->
  let link (i, c) = 
    let rec get_cont visited_ms c = match c with
      | Cexternal mname ->
          (try
            if List.mem mname visited_ms then
              raise Not_found
            else
              get_cont (mname :: visited_ms) (lookup (List.assoc mname ens) i)
          with
            Not_found ->
              raise (Io_module_error (Io_undefined_export_error (i, mname))))
      | c -> c
    in match !c with
      | Cexternal mname -> update_ref ten (i, (get_cont [] (lookup ten i)))
      | _ -> ()
  in
  List.iter link en
 -}
