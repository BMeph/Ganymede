{-# LANGUAGE DeriveDataTypeable #-}
{------------------------------------------------------------
(************************************************************ 
 Ganymede: the main Io interpreter module
   Based on:
   Amalthea, amalthea.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   Translated to Haskell in 2011 by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
-------------------------------------------------------------}

module Main where


import Io.Modules as Module (catch_module_errors, loadAST, load_world_of_ast)
import Io.Interpreter as Interp (catch_interpreter_errors, interpret_expr_in_env)
import Io.Pretty as Pretty (prettyPrint)
import Io.Types
import Io.Util (ganyOptErr, ganyRunErr)

import Prelude hiding (catch, error)
import Control.Exception (Handler(..), catches)
import Data.Data (Data)
import Data.List (foldl')
import Data.Maybe (fromMaybe, fromJust, listToMaybe)
import Data.Typeable (Typeable)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

ganyOpts :: Options
getOpts :: [String] -> IO (Options, [String])
specList :: [OptDescr (Options -> IO Options)]
usageMsg :: String
version :: String

data Options = Options 
  { inputOpt   :: Maybe FilePath
  , ppOpt      :: Bool
  , showOpts   :: Bool
  , versionOpt :: Bool
  , traceOpt   :: Bool
  , pLoadOpt   :: IoImports
  , varSuptOpt :: Bool
  } deriving (Show, Data, Typeable)
-- Informative program values:
version = "0.0.0.6"

usageMsg =
  "Usage: ganymede [options] <sourcefile>\n\
  \Runs the Io source file.\n\
  \Options are:"

ganyOpts = Options
  { inputOpt   = Nothing
  , ppOpt      = False
  , showOpts   = False
  , versionOpt = False
  , traceOpt   = False
  , pLoadOpt   = [("prelude", Nothing)]
  , varSuptOpt = False
  }

-- (* Argument handling *)
specList =
  [ Option ['h','?']     ["help"]
      (NoArg (\_ -> putStrLn (usageInfo usageMsg specList)
                       >> exitSuccess))
      " Show this help text"
  , Option ['d','t']     ["debug","trace"]
      (NoArg (\ opts -> return$ opts { traceOpt = True }))
      " print execution trace"
  , Option ['V'    ]     ["version"]
      (NoArg (\_ -> putStrLn ("Ganymede, version "++ version)
                       >> exitSuccess))
      " print the Ganymede version number"
  , Option ['c','n']     ["clean","noprelude"]
      (NoArg (\opts -> return$ opts { pLoadOpt = [] }))
      " prevent the prelude from being loaded"
  , Option ['P'    ]     ["Prelude"]
      (OptArg ((\f opts -> return$ opts { pLoadOpt = [(f, Nothing)] }) .
          fromMaybe "prelude")
                "FILE")
      " load FILE as prelude"
  , Option ['p'    ]    ["pretty"]
      (NoArg (\opts -> return$ opts { ppOpt = True }))
      " pretty-print the source file"
  , Option ['m'    ]    ["mutable","vars"]
      (NoArg (\ opts -> return$ opts { varSuptOpt = True }))
      " enable mutable module variables (unpure)"
  ]

getOpts args = let gErr = ganyOptErr usageMsg specList in
  case getOpt Permute specList args of
    (o,n, [] ) -> do {
      opts <- foldl' (>>=) (return ganyOpts) $
                (\opts -> return$ opts { inputOpt = listToMaybe n }) : o;
      if null n
        then gErr "missing argument <sourcefile>\n"
        else return (opts, drop 1 n) }
    (_,_,errs) -> gErr (concat errs)

{- --Loads dependent modules and interprets the program -}
build_n_run :: (String, IoAST) -> (Bool, Bool) -> IO ()
-- ^ The (String, IoAST) pair denotes the filename, and its associates Io AST;
--  the pair of Bools denote mutables and program tracing, respectively.
build_n_run p@(_, (_, _, _, ex)) (varStatus, traceStatus) = do {
  (_,env,_) <- Module.catch_module_errors -- Modules
        Module.load_world_of_ast traceStatus p ;
  Interp.catch_interpreter_errors        -- Interp
        Interp.interpret_expr_in_env (env, ex, (varStatus, traceStatus)) }
{--}

main :: IO ()
main = do
  
  (opts,_) <- getOpts =<< getArgs
--  print opts
  let filename = fromJust (inputOpt opts)
  ast <- Module.catch_module_errors  Module.loadAST (pLoadOpt opts) filename
  if ppOpt opts
    then putStrLn (Pretty.prettyPrint ast "\n")
    else (do
      build_n_run (filename, ast) (varSuptOpt opts, traceOpt opts)
      hFlush stdout
     `catches` [ Handler (\(IoError err) -> ioError (userError err))
 -- if all else fails...
               , Handler  ganyRunErr
               ])

{-
(************************************************************ 
 Amalthea main                                                
   Amalthea, amalthea.ml                                      
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)
--
open Io_types

(* The version of Amalthea *)
let amalthea_version = 0.875


(* Flags *)
let filename = ref ""
let pretty_flag = ref false
let version_flag = ref false

(* Argument handling *)
let usage_msg =
  "Usage: amalthea [options] <sourcefile>\n\
   Runs the Io source file.\n\
   Options are:"
let speclist =
  [("-noprelude", Arg.Unit (fun _ -> Io_module.default_imports := []), " prevent the prelude from being loaded");    
   ("-prelude <name>", Arg.String (fun n -> Io_module.default_imports := [(n,None)]), " load a different module as prelude");
   ("-pretty", Arg.Set pretty_flag, " pretty print the source file");
   ("-trace", Arg.Set Io_interpreter.trace_flag, " print execution trace");
   ("-vars", Arg.Set Io_env.variable_flag, " enable mutable module variables (unpure)");   
   ("-version", Arg.Set version_flag, " print the Amalthea version number")]


(* Loads dependant modules and interprets the program *)
let build_n_run : string * io_ast -> unit
= function (_, (_, _, _, ex)) as p ->
  let world = Io_module.catch_module_errors
      Io_module.load_world_of_ast p in
  Io_interpreter.catch_interpreter_errors
    Io_interpreter.interpret_expr_in_world (world, ex)


(* And the program starts here *)
let _ =
  Arg.parse speclist (fun f -> filename := f) usage_msg;
  if !version_flag then
    print_string ("Amalthea version " ^ string_of_float amalthea_version
		   ^ "")
  else 
    if !filename = "" then
      (print_endline "Amalthea: missing argument sourcefile";
      Arg.usage speclist usage_msg)
    else
      try
        let ast = Io_module.catch_module_errors Io_module.load_ast !filename  in
        if !pretty_flag then
          print_string (Io_pretty.pretty_print ast)
        else
          build_n_run (!filename, ast);
          flush stdout
      with
        | Sys_error error -> print_endline ("Amalthea: " ^ error)
        | Io_error error -> print_endline error
 -}
  



