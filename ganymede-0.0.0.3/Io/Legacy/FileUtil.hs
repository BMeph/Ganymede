{------------------------------------------------------------
 module Io.FileUtil utility module
   Based on:
   Amalthea, fileutil.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   translated to Haskell in 2011 by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
-------------------------------------------------------------}

module Io.FileUtil where

import Control.Exception (throwIO)
import Control.Monad (filterM)
import System.Directory ( doesFileExist, getDirectoryContents
                        , getPermissions, Permissions(..))
import System.FilePath ((</>), FilePath)
import System.IO.Error (doesNotExistErrorType,mkIOError)
-- (* Misc utility functions relating to files *)

find_file :: FilePath -> FilePath -> IO FilePath
find_file dir_name file_name = fileSearch file_name [dir_name] where
  fileSearch :: FilePath -> [FilePath] -> IO FilePath
  fileSearch fName [] = throwIO (mkIOError doesNotExistErrorType "find_file" Nothing (Just fName))
  fileSearch fName dNames = do
    dirs <- filterM (doesFileExist . (</> file_name)) dNames
    if null dirs
      then do
        moreDirs <- mapM getSubDirs dNames
        fileSearch fName (concat moreDirs)
      else return (head dirs </> file_name)
  getSubDirs :: FilePath -> IO [FilePath]
  getSubDirs dir = do
    dirsAndFiles <- getDirectoryContents dir
    dirs <- filterM isDirectory dirsAndFiles
    return (filter (\d -> (d /= ".") && (d /= "..")) dirs)
  isDirectory :: FilePath -> IO Bool
  isDirectory f = fmap (\pers -> searchable pers && not (executable pers)) (getPermissions f)

{- original Ocaml:
(* Misc utility functions relating to files *)

let rec find_file : string -> string -> string
= fun dir_name file_name ->
  if Sys.file_exists (Filename.concat dir_name file_name) then
    Filename.concat dir_name file_name
  else
    let dir = Unix.opendir dir_name in
    let path = ref "" in
      (try
        while !path = "" do
          let entry = Unix.readdir dir in
          match entry with
            | "." -> ()
            | ".." -> ()
            | _ ->
              let entry = Filename.concat dir_name entry in
              match (Unix.stat entry).Unix.st_kind with
                | Unix.S_DIR ->
                  (try
                    path := find_file entry file_name
                  with
                    | Unix.Unix_error (Unix.ENOENT, _, _) -> ())
                | _ -> ()
        done
      with
        End_of_file -> ());
      if (!path) = "" then
        raise (Unix.Unix_error (Unix.ENOENT, "find_file", file_name))
      else
        !path

 -}        
