module Socket.Debug
  ( debug
  , whenDebugging
  ) where

import System.IO (hFlush,stdout)

debug :: String -> IO ()
debug str = do
  putStrLn str
  hFlush stdout

whenDebugging :: IO () -> IO ()
whenDebugging = id
