module Io.FuncUtil where

import Control.Monad.ST (runST)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Typeable (Typeable(..))

-- import Control.Exception (throw)
-- import System.IO.Unsafe (unsafePerformIO)

-- For-convenience STRef-manipulating functions
ref = runST . newSTRef
fetch = runST . readSTRef
(<--) = writeSTRef
(<-<) = modifySTRef

-- (* Helper functions *)   
subStr :: Int -> Int -> [a] -> [a]
subStr e b xs = take e . drop b $ xs

print_ = putStr . show

int_of_string = fmap ((+0) . fst) . listToMaybe . reads
