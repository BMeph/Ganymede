{-# LANGUAGE NoMonomorphismRestriction #-}

module Io.FuncUtil where

import Io.Types

import Control.Monad (forever, when)
import Data.IORef (modifyIORef, IORef, newIORef, readIORef, writeIORef)
import Data.List (genericDrop, genericTake)
import Data.Maybe (listToMaybe) -- for string_to_int
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
(<-<) :: IORef a -> (a -> a) -> IO ()

ref = unsafePerformIO . newIORef
fetch = unsafePerformIO . readIORef
(<--) = writeIORef
(<-<) = modifyIORef

-- (* Helper functions *)   
subStr :: Integer -> Integer -> [a] -> [a]
subStr b e = genericDrop b . genericTake e

(.>) :: (Functor m) => m b -> a -> m a
-- "mb .> a" uses mb's effects, and returns a
(.>) = (.const).flip fmap

print_ :: (Show a) => a -> IO ()
print_ = putStr . sSpace . show where
   sSpace :: ShowS
   sSpace = (" "++)

int_of_string :: (Read i, Num i) => String -> Maybe i
int_of_string = fmap ((+0) . fst) . listToMaybe . reads

-- while p <action> works like C(++)-style while
while :: (Monad m) => Bool -> m () -> m b
while = (forever.). when

bool :: t -> t -> Bool -> t
bool false  _   False = false
bool   _   true True  = true

ifte :: Bool -> t -> t -> t
ifte p t f = bool f t p
