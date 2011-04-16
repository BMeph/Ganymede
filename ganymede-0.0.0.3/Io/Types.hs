{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
{------------------------------------------------------------
(************************************************************ 
 Io Types
   Based on:
   Amalthea, io_types.ml io_types.mli
   Martin Sandin (d97masa@dtek.chalmers.se)  
   translated to Haskell by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
   
 ************************************************************)
-------------------------------------------------------------}
module Io.Types
     ( IoID
     , Pos
     , EnvFilter(..)
     , IORef
     , IoExpr(..)
     , IoDecl
     , IoImport
     , IoImports
     , IoExports
     , IoAST
     , IoAssoc 
     , IoVAssoc 
     , IoAssocs
     , IoVAssocs 
     , IoEnv
     , PrimFunc
     , IoCont (..)
     , IoModuleName
     , IoModuleInterface
     , IoModule
     , IoWorld
     , IoError (..)
     , IoInterpreterError (..)
     , IoModuleError (..)
     , IoPrimError (..)
     , IoASTError (..)
     ) where


import Control.Concurrent (ThreadId)
import Control.Exception (Exception(..))
import Data.IORef (IORef)
import qualified Data.Map as M (Map)
import Data.Typeable (Typeable(..))
import Text.ParserCombinators.Parsec (SourcePos)

-- Abstract Syntax Tree
type IoID = String

type Pos = SourcePos

data EnvFilter = EFail | EFLimit [IoID]  deriving (Show, Eq)

data IoExpr =
       ELambda Pos [IoID] IoExpr EnvFilter
     | EGetVar Pos IoID IoID IoExpr EnvFilter
     | EPutVar Pos IoID IoExpr IoExpr EnvFilter
     | EAppl Pos IoID [IoExpr] EnvFilter
     | EId Pos IoID
     | EInt Pos Integer
     | EString Pos String
     | EPrimitive IoID
  deriving (Show, Eq)
     
type IoDecl =  (Pos, IoID, IoExpr)
type IoImport = (IoID, Maybe [IoID])
-- ^ IoImports is a pair of: the module to import; other  modules to import
-- TODO: Figure out what is the diff b/t Nothing and Just [] for the second param.
type IoImports = [IoImport]
type IoExports = [IoID]
type IoAST = (IoImports, IoExports, ([IoDecl], [IoDecl]), IoExpr)


-- (* Interpreter *)
type IoAssoc = (IoID, IoCont)

type IoVAssoc = (IoID, IORef IoCont)

type IoAssocs = M.Map IoID (IORef IoCont)

type IoVAssocs = M.Map IoID (IORef (IORef IoCont))

type IoEnv = (IoAssocs, IoVAssocs)

type PrimFunc = [IoCont] -> (IoCont, [IoCont])

data IoCont =
    CPrim PrimFunc
  | CExpr IoExpr IoEnv Bool
  | CInt Integer
  | CChanIn Integer
  | CChanOut Integer
  | CPar ThreadId
  | CString String
  | CTerminate
  | CLocal IoExpr
  | CExternal IoID


-- (* World *)
type IoModuleName = String

type IoModuleInterface = (IoModuleName, (IoExports, IoImports, IoAST))

type IoModule = (IoModuleName, (IoEnv, (IoExports, IoImports, IoAST)))

type IoWorld = (IoModuleName, IoEnv, [IoModule])


-- (* Exceptions *)
data IoError = IoError String deriving (Eq, Ord, Typeable)
instance Show IoError where
    showsPrec _ (IoError err) = shows err

instance Exception IoError

data IoInterpreterError =
    IoMismatchError String Pos Int Int
  | IoUndefinedError IoID
  deriving (Eq, Ord, Show, Typeable)

data IoModuleError =
    IoParserError Pos
  | IoLexerError Pos
  | IoModuleNotFound String String
  | IoUndefinedExportError IoID IoModuleName
  | IoUndefinedImportError IoID IoModuleName
  deriving (Eq, Ord, Show, Typeable)

data IoPrimError = IoPrimError String deriving (Eq, Ord, Typeable)

instance Show IoPrimError where
    showsPrec _ (IoPrimError err) = showString err

data IoASTError = IoASTError deriving (Eq, Ord, Typeable)

instance Show IoASTError where
    showsPrec _ IoASTError = showString "Could not process the Symbol Tree"

instance Exception IoInterpreterError

instance Exception IoModuleError

instance Exception IoPrimError

instance Exception IoASTError



{- The original OCaML:
(************************************************************ 
 Io Types                                                     
   Amalthea, io_types.mli                                    
   Martin Sandin (d97masa@dtek.chalmers.se)                   
 ************************************************************)


(* Abstract syntax tree *)
type io_id = string

type pos = string * int * int

type en_filter =
  | EFall
  | EFlimit of io_id list
  
type io_expr =
  | Elambda of pos * io_id list * io_expr * en_filter
  | Egetvar of pos * io_id * io_id * io_expr * en_filter
  | Eputvar of pos * io_id * io_expr * io_expr * en_filter
  | Eappl of pos * io_id * io_expr list * en_filter
  | Eid of pos * io_id
  | Eint of pos * int
  | Estring of pos * string
  | Eprimitive of io_id

type io_decl = pos * io_id * io_expr

type io_import = io_id * (io_id list) option

type io_imports = io_import list

type io_exports = io_id list

type io_ast = io_imports * io_exports * (io_decl list * io_decl list) * io_expr


(* Interpreter *)
type io_assoc = io_id * (io_cont ref)

and io_vassoc = io_id * (io_cont ref ref)

and io_env = io_assoc list * io_vassoc list

and io_cont =
  | Cprim of (io_cont list -> (io_cont * io_cont list))
  | Cexpr of io_expr * io_env * bool
  | Cint of int
  | Cstring of string
  | Cterminate
  | Clocal of io_expr
  | Cexternal of io_id


(* World *)
type io_module_name = string

type io_module_interface = io_module_name * (io_exports * io_imports * io_ast)

type io_module = io_module_name * (io_env * (io_exports * io_imports * io_ast))

type io_world = io_module_name * io_env * io_module list


(* Exceptions *)
exception Io_error of string

type io_interpreter_error =
  | Io_mismatch_error of string * pos * (int * int)
  | Io_undefined_error of io_id

exception Io_interpreter_error of io_interpreter_error

type io_module_error =
  | Io_parser_error of pos
  | Io_lexer_error of pos
  | Io_module_not_found of string * string
  | Io_undefined_export_error of io_id * io_module_name
  | Io_undefined_import_error of io_id * io_module_name
  
exception Io_module_error of io_module_error

exception Io_prim_error of string

exception Amalathea_AST_error
 -}
 
