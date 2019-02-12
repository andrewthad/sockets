module Socket.Debug
  ( debug
  , whenDebugging
  ) where

debug :: String -> IO ()
debug = putStrLn

whenDebugging :: IO () -> IO ()
whenDebugging = id
