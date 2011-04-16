{-------------------------------------------------------------
(************************************************************ 
 Io Module System                                             
   Amalthea, io_module.ml                                     
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)
-------------------------------------------------------------}
module Io.Modules where

import Io.Env (build_local_conts, fBind, fVBind, link_external_conts, standard_environment)
import Io.Messages as EMess (parser_error, lexer_error,
                             module_not_found_error,
                             undefined_export_error,
                             undefined_import_error)
import Io.Parser (parseFromFile, parse_io_module)
import Io.Types
import Io.Util (find_file, ref, splitString, uncIns)

import Prelude hiding (catch, lookup)
import Control.Exception (catches, onException, throw, Handler(..))
import Data.Char (toLower)
import Data.List as L (find, foldl', lookup)
import Data.Map as M (empty, insert, toList)
import System.Environment (getEnv)
import System.FilePath (dropFileName)

-- defaultImports :: IORef IoImports


-- (* Returns the filename of a module import id *)
idToFilename :: IoID -> FilePath
idToFilename i = map toLower i ++ ".io"


-- (* Returns the list of ids exported by a module *)
exportedIDs :: IoAST -> [IoID]
exportedIDs (_,[], (ds, _),_) = map (\ (_,i,_) -> i) ds
exportedIDs (_,es,_,_)  = es
  
  
{- Loads an Io source file, parses and lexes it. -}
loadAST :: FilePath -> IO IoAST
loadAST file_path = do {
  anAST <- parseFromFile parse_io_module file_path;  -- Parser
  
  either (\_ -> throw IoASTError) return anAST
  }

{- Finds the full module name, i.e. the path of the module source file,
   from the module filename -}
find_module_name :: IoModuleName -> FilePath -> IO IoModuleName
find_module_name into_name file_name =
  let dir_name = dropFileName into_name in
    (find_file dir_name file_name `onException` do
      lib_path <- getEnv "IOLIB"
      let lib_paths = splitString ';' lib_path
      mapM_ (\p -> find_file p file_name) lib_paths)
    `onException` (throw (IoModuleNotFound into_name file_name))

{- Loads module files recursively, and returns their interfaces. These can 
     then be used for building the actual modules including the environments. -}
load_interfaces :: [IoModuleInterface] -> String -> IoAST -> IO [IoModuleInterface]
load_interfaces interfaces imported_name importedAST@(imps, _, _, _) =
  let split_n_foldLM :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
      split_n_foldLM _ a [] = return (a, [])
      split_n_foldLM f a (x:xs) = do {
        (a', y) <- f a x;
        (aa, ys) <- split_n_foldLM f a' xs;
        return (aa, y:ys) }

      load_interfaces_ :: String -> [IoModuleInterface] -> IoImport ->
        IO ([IoModuleInterface], IoImport)
      load_interfaces_ importing_name iFaces (imported_id, imps_by_imp) = do {  
        name <- find_module_name importing_name (idToFilename imported_id);
        fmap (\x -> (x, (name, imps_by_imp))) $
          case L.lookup name iFaces of
            Nothing -> loadAST name >>= (load_interfaces iFaces name)
            _       -> return iFaces }

      exps = exportedIDs importedAST
      is = (imported_name, (exps, [], importedAST)):interfaces -- [IoModuleInterface]
  in do
    { (is', imps') <- split_n_foldLM (load_interfaces_ imported_name) is imps
    ; return . M.toList . M.insert imported_name (exps, imps', importedAST) $
             foldr uncIns M.empty is'
    }
 
{- Builds an environment for a module -}
build_env :: [IoModuleInterface] -> IoModuleName -> IoEnv
build_env interfaces name = foldr Io.Env.fVBind en'
    (map (\ (_, vi, ex) -> (vi,ref (CLocal ex))) vds) where
  Just (_,imps,(_, _, (ds, vds),_)) = L.lookup name interfaces
  en  = foldl' imp_assocs Io.Env.standard_environment imps
  en' = foldr (Io.Env.fBind .  (\ (_, i, ex) -> (i, CLocal ex))) en ds
  imp_assocs env (imp_name, is) = foldr (\ i -> Io.Env.fBind (i, CExternal imp_name)) env imp_ids where
    Just (expIDs, _, _) = L.lookup imp_name interfaces
    imp_ids = case is of
                Nothing  -> expIDs
                Just ids -> case L.find (`notElem` expIDs) ids of
                    Nothing -> ids
                    Just i -> throw (IoUndefinedImportError i imp_name)

{- Builds a list of modules from a list of interfaces, creating environments
   and linking the continuations between them. -}
build_modules :: Bool -> [IoModuleInterface] -> IO [IoModule]
build_modules traceP interfaces = do
  { mapM_ (Io.Env.build_local_conts traceP) ens
  ; mapM_ (Io.Env.link_external_conts enmp) ens
  ; return . map to_mod $ zip interfaces ens
  } where
  to_mod = (\((name, spec), en) -> (name, (en, spec)))
  names = map fst interfaces
  ens = map (build_env interfaces) names
  enmp = zip names ens

{- Uses an AST to construct a world in which continuations can be evaluated,
   loading external modules in the process. -}
load_world_of_ast :: Bool -> (IoModuleName, IoAST) -> IO IoWorld
load_world_of_ast traceP (name, ast) = do
  { is <- load_interfaces [] name ast
  ; ms <- build_modules traceP is
  ; let Just (en, _) = L.lookup name ms
  ; return (name, en, ms)
  }

{- Catches and prints error messages from the module system -}
catch_module_errors :: (a -> IO b) -> a -> IO b
catch_module_errors f a =
  catches (f a) $
    [ Handler (\(IoParserError pos) ->
      throw (IoError (EMess.parser_error pos)))
    , Handler (\(IoLexerError pos) ->
      throw (IoError (EMess.lexer_error pos)))
    , Handler (\(IoModuleNotFound importer imported) ->
      throw (IoError (EMess.module_not_found_error importer imported)))
    , Handler (\(IoUndefinedExportError i mname) ->
      throw (IoError (EMess.undefined_export_error i mname)))
    , Handler (\(IoUndefinedImportError i mname) ->
      throw (IoError (EMess.undefined_import_error i mname)))
    ]


{- Original ML:
      
open Io_types;;
open String;;

let default_imports = ref [("prelude", None)]

(* Returns the filename of a module import id *)
let filename_of_id
  : io_id -> string
= function i -> lowercase i ^ ".io"


(* Returns the list of ids exported by a module *)
let exported_ids
  : io_ast -> io_id list
= function 
  | (_,[], (ds, _),_) -> List.map (function (_,i,_) -> i) ds
  | (_,es,_,_)  -> es
  
  
(* Loads an Io source file, parses and lexes it. *)
let load_ast (file_path : string)
  : io_ast
= Io_lexer.reset_pos file_path;
  Io_parser_state.reset_pos file_path;
  Parsing.clear_parser ();
  let file_chan = open_in file_path in
  let (imps, exps, dcls, expr) = Io_parser.parse_io_module
    Io_lexer.lex_source (Lexing.from_channel (file_chan)) in
  close_in file_chan;
  (!default_imports @ imps, exps, dcls, expr)


(* Finds the full module name, i.e. the path of the module source file,
   from the module filename *)
let split_string
  : char -> string -> string list
= fun div str -> let rec
    split_it from =
      try
        let t = index_from str from div in
        (sub str from (t - from))::(split_it (t + 1))
      with
        Not_found -> [sub str from ((length str) - from)]
  in
  split_it 0

let find_module_name
  : io_module_name -> string -> io_module_name
= fun into_name file_name ->
  let dir_name = Filename.dirname into_name in
  try
    try
      Fileutil.find_file dir_name file_name
    with
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
        let lib_paths = split_string ';' (
          try
            Sys.getenv "IOLIB"
          with
            Not_found ->
              raise (Unix.Unix_error (Unix.ENOENT, "find_module_name", file_name))
        ) in
        let rec try_paths = (function
          | [] -> raise (Unix.Unix_error (Unix.ENOENT, "find_module_name", file_name))
          | p::ps ->
            try
              Fileutil.find_file p file_name
            with
              Unix.Unix_error _ -> try_paths ps) in
        try_paths lib_paths
              
  with
    | Unix.Unix_error (Unix.ENOENT, _,_) ->
      raise (Io_module_error (Io_module_not_found (into_name, file_name)))
    | Not_found ->
      raise (Io_module_error (Io_module_not_found (into_name, file_name)))




(* Loads module files recursively, and returns their interfaces. These can 
   then be used for building the actual modules including the environments. *)
let rec load_interfaces
  : io_module_interface list -> string -> io_ast -> io_module_interface list
= fun interfaces imported_name ((imps, _, _, _) as imported_ast) ->

    let rec split_n_fold
      : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a
    = fun f a bs -> match bs with
      | [] -> ([], a)
      | b::bs ->
        let c, a = f a b in
        let cls, a = split_n_fold f a bs in
        (c::cls, a)
    in
    
    let rec load_interfaces_
      : string -> io_module_interface list ->
        io_import -> io_import * (io_module_interface list)
    = fun importing_name interfaces (imported_id, imps_by_imp) ->
        let name =
          find_module_name importing_name (filename_of_id imported_id) in
        if List.mem_assoc name interfaces then
          ((name, imps_by_imp), interfaces)
        else
          let ast = load_ast name in
          ((name, imps_by_imp), load_interfaces interfaces name ast)
    in
    
    let exps = exported_ids imported_ast in
    let is = (imported_name, (exps, [], imported_ast))::interfaces in
    let imps, is = split_n_fold (load_interfaces_ imported_name) is imps in
    (imported_name, (exps, imps, imported_ast))::(List.remove_assoc imported_name is)


(* Builds an environment for a module *)
let build_env
  : io_module_interface list -> io_module_name -> io_env
= fun interfaces name ->
  let (_,imps,(_, _, (ds, vds),ex)) = List.assoc name interfaces in
  let imp_assocs en (imp_name, is) =
    let (exp_ids, _, _) = List.assoc imp_name interfaces in
    let imp_ids = match is with
      | None -> exp_ids
      | Some is -> List.map (fun i ->
            try List.find (fun j -> j = i) exp_ids with
              | Not_found -> raise (Io_module_error (Io_undefined_import_error (i, imp_name))) 
          ) is in
    List.fold_left (fun en i -> Io_env.bind en (i, Cexternal imp_name)) en imp_ids
  in
  let en = List.fold_left imp_assocs Io_env.standard_environment imps in
  let en = List.fold_left Io_env.bind en (List.map (function (_, i, ex) -> (i, Clocal ex)) ds) in
  List.fold_left Io_env.vbind en (List.map (function (_, vi, ex) -> (vi,ref (Clocal ex))) vds)


(* Builds a list of modules from a list of interfaces, creating environments
   and linking the continuations between them. *)
let build_modules
  : io_module_interface list -> io_module list
= fun interfaces ->
  let names = List.map fst interfaces in
  let ens = List.map (build_env interfaces) names  in
  List.iter Io_env.build_local_conts ens;
  let enmp = List.combine names ens in
  List.iter (Io_env.link_external_conts enmp) ens;
  let to_mod = function ((name, spec), en) -> (name, (en, spec)) in
  List.map to_mod (List.combine interfaces ens)
  

(* Uses an ast to construct a world in which continuations can be evaluated,
   loading external modules in the process. *)
let load_world_of_ast
  : io_module_name * io_ast -> io_world
= function (name, ast) ->
  let is = load_interfaces [] name ast in
  let ms = build_modules is in
  let en, _ = List.assoc name ms in
  (name, en, ms)
  

(* Catches and prints error messages from the module system *)
let catch_module_errors
= fun f a ->
  try f a with
    | Io_module_error (Io_parser_error pos) ->
      raise (Io_error (Io_messages.parser_error pos))
    | Io_module_error (Io_lexer_error pos) ->
      raise (Io_error (Io_messages.lexer_error pos))
    | Io_module_error (Io_module_not_found (importer, imported)) ->
      raise (Io_error (Io_messages.module_not_found_error importer imported))
    | Io_module_error (Io_undefined_export_error (id, mname)) ->
      raise (Io_error (Io_messages.undefined_export_error id mname))
    | Io_module_error (Io_undefined_import_error (id, mname)) ->
      raise (Io_error (Io_messages.undefined_import_error id mname))

 -}
      
