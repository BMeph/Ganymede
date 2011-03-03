{-# LANGUAGE NoMonomorphismRestriction #-}
{------------------------------------------------------------
 module Io.FileUtil utility module
   Based on:
   Amalthea, fileutil.ml
   Martin Sandin (d97masa@dtek.chalmers.se)
   translated to Haskell in 2011 by Walt "BMeph" Rorie-Baety
     (black.meph@gmail.com)
 ************************************************************)
-------------------------------------------------------------}

module Io.Util where

import Io.Types

import Control.Applicative ((<*))
import Control.Exception (Exception, throwIO)
import Control.Monad (filterM, forever, when)
--import Data.HashMap.Lazy as M (insert)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (genericDrop, genericTake)
import Data.Maybe (listToMaybe) -- for string_to_int
import System.Directory ( doesFileExist, getDirectoryContents
                        , getPermissions, Permissions(..))
import System.FilePath ((</>), FilePath)
import System.IO.Error (doesNotExistErrorType,mkIOError)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec.Pos (initialPos, newPos)
-- import Control.Exception (throw)

-- (* Util *)
freshPos :: Pos
freshPos = newPos "" 0 0

initialPos :: String -> Pos
initialPos = Text.Parsec.Pos.initialPos

showsPos :: Pos -> ShowS
showsPos = shows

-- For-convenience IORef-manipulating functions
ref :: a -> IORef a
fetch :: IORef a -> a
(<--) :: IORef a -> a -> IO ()
(=:) :: IORef a -> a -> ()
(<-<) :: IORef a -> (a -> a) -> IO ()

ref = unsafePerformIO . newIORef
fetch = unsafePerformIO . readIORef
(<--) = writeIORef
ioref =: a = unsafePerformIO (ioref <-- a)
(<-<) = modifyIORef

-- (* Helper functions *)   
subStr :: Integer -> Integer -> [a] -> [a]
subStr b e = genericDrop b . genericTake e

(.>) :: (Functor m) => m b -> a -> m a
-- "mb .> a" uses mb's effects, and returns a
(.>) = (.const).flip fmap

print_ :: (Show a) => a -> IO ()
print_ = (>> (putStr " ")). putStr . show

int_of_string :: (Read i, Num i) => String -> Maybe i
int_of_string = fmap ((+0) . fst) . listToMaybe . reads

-- bool: like maybe, foldr or either, it deconstructs a Bool into
--  some other type of your choice.
bool :: t -> t -> Bool -> t
bool false  _   False = false
bool   _   true True  = true

ifte :: Bool -> t -> t -> t
ifte p t f = bool f t p

showBracket :: ShowS -> ShowS
showBracket p = showChar '[' . p . showChar ']'

splitString :: Char -> String -> [String]
splitString divider string =
  case dropWhile (/= divider) string of
    "" -> []
    s' -> w : splitString divider (drop 1 s'')
          where (w, s'') =
                 break (/= divider) s'

-- uncIns = uncurry M.insert
-- first  f (x, y) = (f x, y)
-- second g (x, y) = (x, g y)

{- Misc utility functions relating to files -}

find_file :: FilePath -> FilePath -> IO FilePath
find_file dir_name file_name = fileSearch file_name [dir_name]

fileSearch :: FilePath -> [FilePath] -> IO FilePath
fileSearch fName dNames = fSF dNames where
 fSF [] = throwIO (mkIOError doesNotExistErrorType "find_file" Nothing (Just fName))
 fSF ds = do
  foundDirs <- filterM doesFileExist $ (map (</> fName) ds)
  if null foundDirs
    then do
      moreDirs <- mapM getSubDirs ds
      fSF (concat moreDirs)
    else return (head foundDirs)

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
