module Socket.Debug
  ( debug
  , whenDebugging
  , debugging
  ) where

import System.IO (hFlush,stdout)

debug :: String -> IO ()
debug str = do
  putStrLn str
  hFlush stdout

whenDebugging :: IO () -> IO ()
whenDebugging = id

debugging :: Bool
debugging = True
