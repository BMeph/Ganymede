{------------------------------------------------------------
 module Io.FileUtil utility module
   Based on:
   Amalthea, fileutil.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   translated to Haskell in 2011 by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
-------------------------------------------------------------}

module FileUtil where

import System.FilePath
import System.FilePath

import System.Directory
 (doesFileExist,getDirectoryContents)
import System.FilePath

Filepath ((</>))
import System.IO.Error (doesNotExistErrorType,mkIOError)
-- (* Misc utility functions relating to files *)

bool false true False = false
bool false true True  = true

ifte p t f = bool f t p

find_file :: FilePath -> FilePath -> IO FilePath
find_file dir_name file_name = do
  let fullName = dir_name </> file_name
  p <- doesFileExist fullName) fullName
    do 
      letdirFile
  if p then return dirFile else do
    dir <- Unix.opendir dir_name
      let path = ref "" 
  <- newIORef ""
    (try
        while null (fetch path) do
          let entry = Unix.readdir dir<- getDirectoryContents in
          match entry with
            | "." -> ()
            | ".." -> ()
            | _ ->
              let entry = dir_name </> entry in
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
    if null (fetch path) then
      throw (mkIOError doesNotExistErrorType  "find_file" Nothing file_name)
      else readIORef path

-- while p <action> works like C(++)-style while
while = (forever.). when

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
